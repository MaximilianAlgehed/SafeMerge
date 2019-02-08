{-# LANGUAGE GADTs, DataKinds #-}
module Logic where

import ProgramRep
import Substitutions

data Formula where
  (:&)  :: Formula -> Formula -> Formula
  (:->) :: Formula -> Formula -> Formula 
  (:=:) :: Expr    -> Expr    -> Formula
  (:>)  :: Expr    -> Expr    -> Formula
  FNot  :: Formula -> Formula

instance Show Formula where
  showsPrec p f = case f of
    e0 :=: e1 -> showString $ show e0 ++ " = " ++ show e1
    e0 :>  e1 -> showString $ show e0 ++ " > " ++ show e1
    f0 :&  f1 -> showParen (p >= 4) $ showsPrec 3 f0 . showString " & " . showsPrec 3 f1
    FNot f    -> showParen (p >= 5) $ showString "! " . showsPrec 4 f
    f0 :-> f1 -> showParen (p >= 3) $ showsPrec 3 f0 . showString " -> " . showsPrec 2 f1

instance HasSubst Formula where
  applySubst s f = case f of
    f0 :&  f1 -> applySubst s f0 :&  applySubst s f1
    f0 :-> f1 -> applySubst s f0 :-> applySubst s f1
    e0 :=: e1 -> applySubst s e0 :=:  applySubst s e1
    e0 :>  e1 -> applySubst s e0 :>  applySubst s e1
    FNot f    -> FNot (applySubst s f)

conditionToFormula :: Condition -> Formula
conditionToFormula c = case c of
  c0 :&&: c1  -> conditionToFormula c0 :& conditionToFormula c1
  e0 :>: e1   -> e0 :> e1
  e0 :==: e1  -> e0 :=: e1
  CNot c      -> FNot (conditionToFormula c)

wp :: Statement -> Formula -> Maybe Formula 
wp stmt phi = case stmt of
  SSkip  -> return phi
  x := e -> return $ applySubst (singleton x e) phi
  SSeq s0 s1 -> do
    phi' <- wp s1 phi
    wp s0 phi'
  SIf c s0 s1 -> do
    phi0 <- wp s0 phi
    phi1 <- wp s1 phi
    return $ (conditionToFormula c :-> phi0) :& (FNot (conditionToFormula c) :-> phi1)
  -- Don't handle while loops yet
  SWhile c s -> Nothing
  -- Holes do not have a meaningful semantics
  SHole      -> Nothing
