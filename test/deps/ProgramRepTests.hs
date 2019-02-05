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

instance Arbitrary Condition where
  arbitrary = sized go
    where
      go d = case d of
        0 -> oneof [ (:>:) <$> arbitrary <*> arbitrary, (:==:) <$> arbitrary <*> arbitrary ]
        d -> oneof [ (:>:) <$> arbitrary <*> arbitrary, (:==:) <$> arbitrary <*> arbitrary
                   , (:&&:) <$> go (d `div` 2) <*> go (d `div` 2)
                   , CNot <$> go (d - 1) ]

instance Arbitrary AtomicStatement where
  arbitrary = oneof [ return Skip
                    , (:=) <$> arbitrary <*> arbitrary
                    , WriteArray <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary (Statement NoHole) where
  arbitrary = sized go
    where
      go d = case d of
        0 -> SAtom <$> arbitrary
        d -> oneof [ SAtom <$> arbitrary
                   , SSeq <$> go (d `div` 2) <*> go (d `div` 2)
                   , SIf  <$> arbitrary <*> go (d `div` 2) <*> go (d `div` 2)
                   , SWhile <$> arbitrary <*> go (d `div` 2) ]

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

prop_id :: Statement NoHole -> Edit -> Bool
prop_id s delta = apply s delta == Just (s, delta)

prop_apply_count :: Statement MaybeHole -> Property
prop_apply_count s = forAll (vectorOf (numHoles s) arbitrary) $ \delta -> isJust (apply s delta)
