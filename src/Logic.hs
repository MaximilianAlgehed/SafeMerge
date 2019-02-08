{-# LANGUAGE GADTs #-}
module Logic where

import ProgramRep hiding ((:>:))
import qualified ProgramRep as P
import Substitutions

data Formula where
  (:&:)  :: Formula -> Formula -> Formula
  (:->:) :: Formula -> Formula -> Formula 
  (:=:)  :: Expr    -> Expr    -> Formula
  (:>:)  :: Expr    -> Expr    -> Formula
  FNot   :: Formula -> Formula

instance Show Formula where
  show f = case f of
    f0 :&:  f1 -> show f0 ++ " & "  ++ show f1
    f0 :->: f1 -> show f0 ++ " -> " ++ show f1
    e0 :=:  e1 -> show e0 ++ " = "  ++ show e1
    e0 :>:  e1 -> show e0 ++ " > "  ++ show e1
    FNot f     -> "!(" ++ show f ++ ")"

instance HasSubst Formula where
  applySubst s f = case f of
    f0 :&:  f1 -> applySubst s f0 :&:  applySubst s f1
    f0 :->: f1 -> applySubst s f0 :->: applySubst s f1
    e0 :=:  e1 -> applySubst s e0 :=:  applySubst s e1
    e0 :>:  e1 -> applySubst s e0 :>:  applySubst s e1
    FNot f     -> FNot (applySubst s f)

wpAtomic :: AtomicStatement -> Formula -> Formula
wpAtomic a phi = case a of
  Skip   -> phi
  x := e -> applySubst (singleton x e) phi

conditionToFormula :: Condition -> Formula
conditionToFormula c = case c of
  c0 :&&: c1  -> conditionToFormula c0 :&: conditionToFormula c1
  e0 P.:>: e1 -> e0 :>: e1
  e0 :==: e1  -> e0 :=: e1
  CNot c      -> FNot (conditionToFormula c)

wp :: Statement NoHole -> Formula -> Maybe Formula 
wp stmt phi = case stmt of
  SAtom a    -> Just (wpAtomic a phi)
  SSeq s0 s1 -> do
    phi' <- wp s1 phi
    wp s0 phi'
  SIf c s0 s1 -> do
    phi0 <- wp s0 phi
    phi1 <- wp s1 phi
    return $ (conditionToFormula c :->: phi0) :&: (FNot (conditionToFormula c) :->: phi1)
  -- Don't handle while loops yet
  SWhile c s -> Nothing
