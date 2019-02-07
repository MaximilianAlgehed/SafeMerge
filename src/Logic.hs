{-# LANGUAGE GADTs #-}
module Logic where

import ProgramRep

data Formula where
  (:&:) :: Formula -> Formula -> Formula
  (:=:) :: Expr    -> Expr    -> Formula
  (:>:) :: Expr    -> Expr    -> Formula
  FNot  :: Formula -> Formula
