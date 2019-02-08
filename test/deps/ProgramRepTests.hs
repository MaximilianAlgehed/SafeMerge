{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts #-}
module ProgramRepTests where

import Test.QuickCheck
import Data.Maybe
import qualified Data.Set as S

import ProgramRep

binOpShrink :: Arbitrary a => (a -> a -> a) -> a -> a -> [a]
binOpShrink op a0 a1 =  [a0, a1]
                     ++ shrink a0
                     ++ shrink a1
                     ++ [ a `op` a1 | a <- shrink a0 ]
                     ++ [ a0 `op` a | a <- shrink a1 ]

instance Arbitrary Variable where
  arbitrary = Name . (:[]) <$> elements "abcdefghijklmnopqrstuvxyz"

instance Arbitrary Expr where
  arbitrary = sized go
    where
      go d = case d of
        0 -> oneof [ Var <$> arbitrary , Lit <$> arbitrary ]
        d -> oneof [ Var <$> arbitrary , Lit <$> arbitrary
                   , (:+:) <$> go (d `div` 2) <*> go (d `div` 2) ]

  shrink e = case e of
    Var _     -> []
    Lit i     -> Lit <$> shrink i
    e0 :+: e1 -> binOpShrink (:+:) e0 e1

instance Arbitrary Condition where
  arbitrary = sized go
    where
      go d = case d of
        0 -> oneof [ (:>:) <$> arbitrary <*> arbitrary, (:==:) <$> arbitrary <*> arbitrary ]
        d -> oneof [ (:>:) <$> arbitrary <*> arbitrary, (:==:) <$> arbitrary <*> arbitrary
                   , (:&&:) <$> go (d `div` 2) <*> go (d `div` 2)
                   , CNot <$> go (d - 1) ]

  shrink c = case c of
    e0 :>: e1 -> ((e0 :>:) <$> shrink e1)
              ++ ((:>: e1) <$> shrink e0)

    e0 :==: e1 -> ((e0 :==:) <$> shrink e1)
               ++ ((:==: e1) <$> shrink e0)

    c0 :&&: c1 -> binOpShrink (:&&:) c0 c1
    
    CNot c     -> CNot <$> shrink c

instance Arbitrary Statement where
  arbitrary = sized go
    where
      go d = case d of
        0 -> atomic
        d -> oneof [ atomic
                   , return SHole
                   , SSeq <$> go (d `div` 2) <*> go (d `div` 2)
                   , SIf  <$> arbitrary <*> go (d `div` 2) <*> go (d `div` 2)
                   , SWhile <$> arbitrary <*> go (d `div` 2) ]

      atomic = oneof [ return SSkip, (:=) <$> arbitrary <*> arbitrary ]

  shrink s = case s of
    SSeq s0 s1   -> binOpShrink SSeq s0 s1 
    SIf c s0 s1  -> binOpShrink (SIf c) s0 s1 ++ [ SIf c' s0 s1 | c' <- shrink c ] 
    SWhile c s   -> [s]
                 ++ ((\c -> SWhile c s) <$> shrink c)
                 ++ ((SWhile c) <$> shrink s)
    SSkip        -> []
    v := e       -> (v :=) <$> shrink e
    SHole        -> []

newtype NoHoleStatement = NoHole Statement

instance Show NoHoleStatement where
  show (NoHole s) = show s

instance Arbitrary NoHoleStatement where
  arbitrary = NoHole <$> sized go
    where
      go d = case d of
        0 -> atomic
        d -> oneof [ atomic
                   , SSeq <$> go (d `div` 2) <*> go (d `div` 2)
                   , SIf  <$> arbitrary <*> go (d `div` 2) <*> go (d `div` 2)
                   , SWhile <$> arbitrary <*> go (d `div` 2) ]
      atomic = oneof [ return SSkip, (:=) <$> arbitrary <*> arbitrary ]

  shrink (NoHole s) = NoHole <$> shrink s

-- | Check that applying an edit to a program without holes returns
-- just the program the edit.
prop_id :: NoHoleStatement -> Edit -> Bool
prop_id (NoHole s) delta = apply s delta == Just (s, delta)

-- | Check that applying an edit of length |s| (where |s| is the number of holes
-- in s) to a program s consumes all the edits and returns a new program
prop_apply_count :: Statement -> Property
prop_apply_count s = forAll (vectorOf (numHoles s) arbitrary) $ \nhdelta -> isJust (applyEdit s [ s | NoHole s <- nhdelta ])

-- | Check that the set of variables after doing setVariableIndex remains the same but with `_i` added
prop_set_variable_index :: Statement -> Int -> Property
prop_set_variable_index s i = i >= 0 ==> vars (setVariableIndex s i) == (S.map (\(Name n) -> Name $ n ++ "_" ++ show i) (vars s))
