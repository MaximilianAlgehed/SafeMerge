{-# LANGUAGE OverloadedStrings, RebindableSyntax, DataKinds #-}
module Examples where

import ProgramRep
import EmbeddedSyntax
import Logic

import Prelude (($))

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
