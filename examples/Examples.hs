{-# LANGUAGE OverloadedStrings, RebindableSyntax, DataKinds #-}
module Examples where

import ProgramRep
import EmbeddedSyntax
import Logic
import Verification

import Prelude (($), Maybe(..), IO)

p0 :: Statement
p0 = do
  "x" := "y"
  "y" := 1

p1 :: Statement
p1 = do
  "z" := "y"
  "y" := 1
  "x" := "z"

p2 :: Statement
p2 = do
  if "x" :>: "y" & "z" :>: "y" then
    "z" := 6
  else
    "z" := 5
  "z" := "z" + "x"

p3 :: Statement
p3 = do
  "z" := 0
  if "y" :>: "x" then
    while ("y" :>: "x") $ do
      "x" := "x" + 1
      "z" := "z" + 1
  else do
    while ("x" :>: "y") $ do
      "y" := "y" + 1
      "z" := "z" - 1

{- Example 4.2 in the paper -}
e42_S :: Statement
e42_S = do
  hole
  hole

e42_Delta_O, e42_Delta_A, e42_Delta_B, e42_Delta_M :: Edit
e42_Delta_O = [skip, skip]
e42_Delta_A = ["x" := "x" + 1, skip]
e42_Delta_B = [skip, "x" := "x" + 1]
e42_Delta_M = ["x" := "x" + 1, "x" := "x" + 1]

run_e42 :: IO ()
run_e42 = do
  let Just ht = mergeCandidateHoareTriple e42_S e42_Delta_O e42_Delta_A e42_Delta_B e42_Delta_M ["x"]
  verify ht
