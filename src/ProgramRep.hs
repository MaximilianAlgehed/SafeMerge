{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module ProgramRep where

data HasHole where
  Hole   :: HasHole
  NoHole :: HasHole

data Variable = Name String

data Expr where
  Var   :: Variable -> Expr
  Lit   :: Int      -> Expr
  (:+:) :: Expr     -> Expr -> Expr

data Condition where
  (:&&:) :: Condition -> Condition -> Condition
  (:>:)  :: Expr      -> Expr      -> Condition
  (:==:) :: Expr      -> Expr      -> Condition
  CNot   :: Condition -> Condition

data AtomicStatement where
  Skip       :: AtomicStatement
  (:=)       :: Variable -> Expr -> AtomicStatement
  WriteArray :: Variable -> Expr -> Expr -> AtomicStatement

data Statement :: HasHole -> * where 
  SHole  :: Statement Hole
  SAtom  :: AtomicStatement -> Statement h
  SSeq   :: Statement h     -> Statement h -> Statement h
  SIf    :: Condition       -> Statement h -> Statement h -> Statement h
  SWhile :: Condition       -> Statement h -> Statement h
  
type Edit = [Statement NoHole]

apply :: Statement h -> Edit -> Maybe (Statement NoHole, Edit)
-- [.]
apply SHole (s : delta)  = return (s, delta)
-- A
apply (SAtom a) delta    = return (SAtom a, delta)
-- S ; S
apply (SSeq s0 s1) delta = do
  (s0', delta1) <- apply s0 delta
  (s1', delta2) <- apply s1 delta1
  return (SSeq s0' s1', delta2)
-- C ? S0 : S1
apply (SIf c s0 s1) delta = do
  (s0', delta1) <- apply s0 delta
  (s1', delta1) <- apply s1 delta1
  return (SIf c s0' s1', delta1)
-- while(C) S
apply (SWhile c s) delta = do
  (s', delta1) <- apply s delta
  return (SWhile c s', delta1)
apply _ _ = Nothing

applyEdit :: Statement h -> Edit -> Maybe (Statement NoHole)
applyEdit s delta = do
  (s', []) <- apply s delta
  return s'
