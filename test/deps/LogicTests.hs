{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module LogicTests where

import Prelude hiding (return, (>>))

import Test.HUnit ((@=?))
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Logic
import EmbeddedSyntax
import ProgramRep
import Semantics

import ProgramRepTests -- To get the instances

prop_cond_form_sem :: Condition -> Env -> Bool
prop_cond_form_sem c env = condSem env c == formSem env (conditionToFormula c)

formProperties :: [Test]
formProperties =
  [ testProperty "prop_cond_form_sem"  prop_cond_form_sem
  ]

unitWP :: [Test]
unitWP = [ testCase "x := 1 + 1"     $ wp ("x" := 1 + 1)            ("x" === 2)   @=? (Just $ 1 + 1 === 2)
         , testCase "x := 1; y := x" $ wp (do "x" := 1; "y" := "x") ("x" === "y") @=? (Just $ 1 === 1)
         ]
