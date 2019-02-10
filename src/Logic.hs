{-# LANGUAGE GADTs, DataKinds, DeriveDataTypeable #-}
module Logic where

import ProgramRep
import Substitutions

import Data.Generics.Uniplate.Data
import Data.Data
import qualified Data.Set as S

data Formula where
  (:&)  :: Formula -> Formula -> Formula
  (:->) :: Formula -> Formula -> Formula 
  (:=:) :: Expr    -> Expr    -> Formula
  (:>)  :: Expr    -> Expr    -> Formula
  FNot  :: Formula -> Formula
  deriving (Data, Typeable)

instance Show Formula where
  showsPrec p f = case f of
    e0 :=: e1 -> showString $ show e0 ++ " = " ++ show e1
    e0 :>  e1 -> showString $ show e0 ++ " > " ++ show e1
    f0 :&  f1 -> showParen (p >= 4) $ showsPrec 3 f0 . showString " & " . showsPrec 3 f1
    FNot f'   -> showParen (p >= 5) $ showString "! " . showsPrec 4 f'
    f0 :-> f1 -> showParen (p >= 3) $ showsPrec 3 f0 . showString " -> " . showsPrec 2 f1

instance HasSubst Formula where
  applySubst = transformBi . tr
    where
      tr :: Substitution -> Expr -> Expr
      tr = applySubst

conditionToFormula :: Condition -> Formula
conditionToFormula c = case c of
  c0 :&&: c1  -> conditionToFormula c0 :& conditionToFormula c1
  e0 :>: e1   -> e0 :> e1
  e0 :==: e1  -> e0 :=: e1
  CNot c'     -> FNot (conditionToFormula c')

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
  SWhile _ _  -> Nothing
  -- Holes do not have a meaningful semantics
  SHole      -> Nothing

data HoareTriple = Hoare { precondition  :: Formula
                         , program       :: Statement
                         , postcondition :: Formula }

instance Show HoareTriple where
  show h = "{"
         ++ show (precondition h)
         ++ "}\n"
         ++ show (program h)
         ++ "{"
         ++ show (postcondition h)
         ++ "}"

vc :: HoareTriple -> Maybe Formula
vc h = do
  weakestPrecondition <- wp (program h) (postcondition h)
  return $ precondition h :-> weakestPrecondition

programEquivalence :: Statement -> Statement -> [Variable] -> HoareTriple
programEquivalence p0 p1 outputs =
  let p0'  = setVariableIndex p0 0
      p1'  = setVariableIndex p1 1
      vs   = vars p0 `S.union` vars p1
      -- Given initial states
      pre  = foldr (:&) (0 :=: 0) [ (Var . Name $ v ++ "_0") :=: (Var . Name $ v ++ "_1") | Name v <- S.toList vs ]
      -- All the outputs are the same
      post = foldr (:&) (0 :=: 0) [ (Var . Name $ v ++ "_0") :=: (Var . Name $ v ++ "_1") | Name v <- outputs ]
  in Hoare pre (productProgram p0' p1') post
