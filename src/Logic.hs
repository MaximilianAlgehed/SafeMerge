{-# LANGUAGE GADTs #-}
module Logic where

import ProgramRep hiding ((:>:))
import Substitutions

data Formula where
  (:&:) :: Formula -> Formula -> Formula
  (:=:) :: Expr    -> Expr    -> Formula
  (:>:) :: Expr    -> Expr    -> Formula
  FNot  :: Formula -> Formula

instance Show Formula where
  show f = case f of
    f0 :&: f1 -> show f0 ++ " & " ++ show f1
    e0 :=: e1 -> show e0 ++ " = " ++ show e1
    e0 :>: e1 -> show e0 ++ " > " ++ show e1
    FNot f    -> "!(" ++ show f ++ ")"

instance HasSubst Formula where
  applySubst s f = case f of
    f0 :&: f1 -> applySubst s f0 :&: applySubst s f1
    e0 :=: e1 -> applySubst s e0 :=: applySubst s e1
    e0 :>: e1 -> applySubst s e0 :>: applySubst s e1
    FNot f    -> FNot (applySubst s f)

wpAtomic :: AtomicStatement -> Formula -> Formula
wpAtomic a phi = case a of
  Skip   -> phi
  x := e -> applySubst (singleton x e) phi
