module Substitutions where

import ProgramRep

import qualified Data.Map as M

type Substitution = M.Map Variable Expr

singleton :: Variable -> Expr -> Substitution
singleton v e = M.singleton v e

class HasSubst t where
  applySubst :: Substitution -> t -> t

instance HasSubst Expr where
  applySubst s e = case e of
    Var v -> case M.lookup v s of
              Nothing -> Var v
              Just e  -> e
    Lit i -> Lit i
    e0 :+: e1 -> applySubst s e0 :+: applySubst s e1
