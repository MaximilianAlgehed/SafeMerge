-- | This language defines the semantics of our tiny language
module Semantics where

import qualified Data.Map as M

import ProgramRep

type Env = M.Map Variable Int

exprSem :: Env -> Expr -> Maybe Int
exprSem env e = case e of
  Var v     -> M.lookup v env
  Lit i     -> return i
  e0 :+: e1 -> (+) <$> exprSem env e0 <*> exprSem env e1
  e0 :-: e1 -> (-) <$> exprSem env e0 <*> exprSem env e1

condSem :: Env -> Condition -> Maybe Bool
condSem env c = case c of
  c0 :&&: c1 -> (&&) <$> condSem env c0 <*> condSem env c1
  e0 :>:  e1 -> (>)  <$> exprSem env e0 <*> exprSem env e1
  e0 :==: e1 -> (==) <$> exprSem env e0 <*> exprSem env e1
  CNot c'    -> not  <$> condSem env c'

stmtSem :: Env -> Statement -> Maybe Env
stmtSem env s = case s of
  v := e -> do
    ev <- exprSem env e
    return $ M.insert v ev env

  SSeq s0 s1 -> do
    env' <- stmtSem env s0
    stmtSem env' s1

  SWhile c s' -> do
    c' <- condSem env c
    if c' then
      stmtSem env (SSeq s' s)
    else
      return env

  SIf c s0 s1 -> do
    c' <- condSem env c
    if c' then
      stmtSem env s0
    else
      stmtSem env s1

  SSkip -> return env

  -- Holes don't have a meaningful semantics
  SHole -> Nothing
