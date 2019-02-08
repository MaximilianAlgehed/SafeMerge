{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , StandaloneDeriving
           , DeriveDataTypeable
#-}
module ProgramRep where

import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

data Variable = Name String deriving (Eq, Ord, Data, Typeable)

instance Show Variable where
  show (Name s) = s

data Expr where
  Var   :: Variable -> Expr
  Lit   :: Int      -> Expr
  (:+:) :: Expr     -> Expr -> Expr
  deriving (Eq, Data, Typeable)

instance Show Expr where
  show e = case e of
    Var v     -> show v
    Lit i     -> show i
    e0 :+: e1 -> show e0 ++ " + " ++ show e1

-- | `while` and `if` statement conditions.
data Condition where
  (:&&:) :: Condition -> Condition -> Condition
  (:>:)  :: Expr      -> Expr      -> Condition
  (:==:) :: Expr      -> Expr      -> Condition
  CNot   :: Condition -> Condition
  deriving (Eq, Data, Typeable)

infixr 4 :>:

instance Show Condition where
  show c = case c of
    c0 :&&: c1 -> show c0 ++ " & " ++ show c1
    e0 :>: e1  -> show e0 ++ " > " ++ show e1
    e0 :==: e1 -> show e0 ++ " == " ++ show e1
    CNot c     -> "!(" ++ show c ++ ")"

-- | Statements parameterised by an index telling us if they
-- may contain a hole or not
data Statement where 
  SHole  :: Statement
  SSkip  :: Statement
  (:=)   :: Variable -> Expr -> Statement
  SSeq   :: Statement -> Statement -> Statement 
  SWhile :: Condition -> Statement -> Statement 
  SIf    :: Condition -> Statement -> Statement -> Statement
  deriving (Eq, Data, Typeable)

infixl 0 :=

instance Show Statement where
  show = unlines . showL 
    where
    showL :: Statement -> [String]
    showL s = case s of
      SHole   -> ["[.]"]
      SSkip   -> ["skip"]
      v := e  -> [show v ++ " := " ++ show e]
      SSeq s0 s1 -> showL s0 ++ showL s1
      SIf c t f  -> ["if(" ++ show c ++ ")"]
                 ++ map ("  "++) (showL t)
                 ++ ["else"]
                 ++ map ("  "++) (showL f)
      SWhile c l -> ["while(" ++ show c ++ ")"]
                 ++ map ("  "++) (showL l)

type Edit = [Statement]

-- | Apply an Edit `delta` by sequentially inserting the statements
-- of `delta` into the holes in the statement
apply :: Statement -> Edit -> Maybe (Statement, Edit)
-- [.]
apply SHole (s : delta)  = return (s, delta)
-- A
apply SSkip delta        = return (SSkip, delta)
apply (v := e) delta     = return (v := e, delta)
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

-- | Apply an Edit and ensure that all the statements in the edit
-- were used in the statement
applyEdit :: Statement -> Edit -> Maybe Statement
applyEdit s delta = do
  (s', []) <- apply s delta
  return s'

numHoles :: Statement -> Int
numHoles s = length [ () | SHole <- universe s ]

-- | From s and i compute s[V/Vi]
setVariableIndex :: Statement -> Int -> Statement
setVariableIndex s i = transformBi go s
  where
    go (Name n) = Name (n ++ "_" ++ show i)
