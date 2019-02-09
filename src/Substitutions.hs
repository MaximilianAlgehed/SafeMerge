module Substitutions where

import ProgramRep

import qualified Data.Map as M
import Data.Generics.Uniplate.Data

type Substitution = M.Map Variable Expr

singleton :: Variable -> Expr -> Substitution
singleton v e = M.singleton v e

class HasSubst t where
  applySubst :: Substitution -> t -> t

instance HasSubst Expr where
  applySubst s = transform tr
    where
      tr (Var v) = case M.lookup v s of
        Nothing -> Var v
        Just e  -> e
      tr e       = e
