{-# LANGUAGE OverloadedStrings, RebindableSyntax, DataKinds #-}
module Examples where

import ProgramRep
import Logic
import EmbeddedSyntax

import Prelude (($))
import GHC.Exts

p0 :: Statement
p0 = do
  if "x" :>: "y" & "z" :>: "y" then
    "z" := 6
  else
    "z" := 5
  "z" := "z" + "x"

p1 :: Statement
p1 = do
  "z" := 0
  while ("y" :>: "x") $ do
    "x" := "x" + 1
    "z" := "z" + 1
