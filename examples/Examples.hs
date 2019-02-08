{-# LANGUAGE OverloadedStrings, RebindableSyntax, DataKinds #-}
module Examples where

import ProgramRep
import Logic
import EmbeddedSyntax

import GHC.Exts

p0 :: Statement NoHole
p0 = do
  if "x" :>: "y" & "z" :>: "y" then
    "z" := 6
  else
    "z" := 5
  "z" := "z" + "x"
