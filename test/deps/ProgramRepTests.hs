{-# LANGUAGE DataKinds, FlexibleInstances #-}
module ProgramRepTests where

import Test.QuickCheck
import Data.Maybe

import ProgramRep

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
    e0 :+: e1 -> shrink e0 
              ++ shrink e1
              ++ ((:+: e1) <$> shrink e0)
              ++ ((e0 :+:) <$> shrink e1)

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

    e0 :&&: e1 -> ((e0 :&&:) <$> shrink e1)
               ++ ((:&&: e1) <$> shrink e0)
    
    CNot c     -> CNot <$> shrink c

instance Arbitrary AtomicStatement where
  arbitrary = oneof [ return Skip
                    , (:=) <$> arbitrary <*> arbitrary
                    , WriteArray <$> arbitrary <*> arbitrary <*> arbitrary ]

  shrink a = case a of
    Skip               -> []
    v := e             -> (v :=) <$> shrink e
    WriteArray v e0 e1 -> (WriteArray v e0 <$> shrink e1)
                       ++ ((\e -> WriteArray v e e1) <$> shrink e0)

shrinkStmt :: Statement h -> [Statement h]
shrinkStmt s = case s of
  SAtom a      -> SAtom <$> shrink a
  SSeq s0 s1   -> [s0, s1]
               ++ shrinkStmt s0
               ++ shrinkStmt s1
               ++ ((SSeq s0)         <$> shrinkStmt s1)
               ++ ((\s -> SSeq s s1) <$> shrinkStmt s0)
  SIf c s0 s1  -> [s0, s1]
               ++ ((\c -> SIf c s0 s1) <$> shrink c)
               ++ ((\s -> SIf c s s1) <$> shrinkStmt s0)
               ++ ((\s -> SIf c s0 s) <$> shrinkStmt s1)
  SWhile c s   -> [s]
               ++ ((\c -> SWhile c s) <$> shrink c)
               ++ ((SWhile c) <$> shrinkStmt s)

instance Arbitrary (Statement NoHole) where
  arbitrary = sized go
    where
      go d = case d of
        0 -> SAtom <$> arbitrary
        d -> oneof [ SAtom <$> arbitrary
                   , SSeq <$> go (d `div` 2) <*> go (d `div` 2)
                   , SIf  <$> arbitrary <*> go (d `div` 2) <*> go (d `div` 2)
                   , SWhile <$> arbitrary <*> go (d `div` 2) ]
  shrink = shrinkStmt

instance Arbitrary (Statement MaybeHole) where
  arbitrary = sized go
    where
      go d = case d of
        0 -> oneof [ SAtom <$> arbitrary , return SHole ]
        d -> oneof [ SAtom <$> arbitrary
                   , return SHole
                   , SSeq <$> go (d `div` 2) <*> go (d `div` 2)
                   , SIf  <$> arbitrary <*> go (d `div` 2) <*> go (d `div` 2)
                   , SWhile <$> arbitrary <*> go (d `div` 2) ]
  shrink = shrinkStmt

prop_id :: Statement NoHole -> Edit -> Bool
prop_id s delta = apply s delta == Just (s, delta)

prop_apply_count :: Statement MaybeHole -> Property
prop_apply_count s = forAll (vectorOf (numHoles s) arbitrary) $ \delta -> isJust (applyEdit s delta)
