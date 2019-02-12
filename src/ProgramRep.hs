{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , StandaloneDeriving
           , DeriveDataTypeable
#-}
module ProgramRep where

import qualified Data.Set as S
import Data.Data
import Data.Generics.Uniplate.Data
import GHC.Exts

data Variable = Name String deriving (Eq, Ord, Data, Typeable)

instance Show Variable where
  show (Name s) = s

instance IsString Variable where
  fromString = Name

instance Semigroup Variable where
  Name n <> Name m = Name (n ++ m)

instance Monoid Variable where
  mempty = Name []

data Expr where
  Var   :: Variable -> Expr
  Lit   :: Int      -> Expr
  (:+:) :: Expr     -> Expr -> Expr
  (:-:) :: Expr     -> Expr -> Expr
  deriving (Eq, Data, Typeable)

instance Show Expr where
  showsPrec p e = case e of
    Var v     -> showString $ show v
    Lit i     -> showString $ show i
    e0 :+: e1 -> showParen (p >= 6) $ showsPrec 6 e0 . showString " + " . showsPrec 5 e1
    e0 :-: e1 -> showParen (p >= 6) $ showsPrec 6 e0 . showString " - " . showsPrec 5 e1

instance Num Expr where
  (+)         = (:+:)
  (*)         = undefined
  (-)         = (:-:)
  negate      = undefined
  signum      = undefined
  abs         = undefined
  fromInteger = Lit . fromInteger

instance IsString Expr where
  fromString = Var . fromString

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
    CNot c'    -> "!(" ++ show c' ++ ")"

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
      SWhile c l -> ["while (" ++ show c ++ ")"]
                 ++ map ("  "++) (showL l)

type Edit = [Statement]

-- | Apply an Edit `delta` by sequentially inserting the statements
-- of `delta` into the holes in the statement
apply :: Statement -> Edit -> Maybe (Statement, Edit)
-- [.]
apply SHole (s : delta)  = return (s, delta)
apply SHole []           = Nothing
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
  (s1', delta2) <- apply s1 delta1
  return (SIf c s0' s1', delta2)
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

-- | Compute the variables from a statement
vars :: Statement -> S.Set Variable
vars = S.fromList . universeBi 

-- | Flatten a tree shape of statements (built from `SSeq`)
flatten :: Statement -> [Statement]
flatten = filter (/= SSkip) . go
  where
    go s = case s of
      SSeq s0 s1 -> go s0 ++ go s1
      _          -> [s]

-- | Sequence a list of statements
sseq :: [Statement] -> Statement
sseq = foldr SSeq SSkip

-- | Semantically equivalent to `s0 ; s1` given that `vars s0` is disjoint from `vars s1`
productProgram :: Statement -> Statement -> Statement
productProgram st0 st1 = go (flatten st0) (flatten st1)
  where
    -- Following Fig. 8 in the Sousa et al. paper
    go :: [Statement] -> [Statement] -> Statement
    go ss0 ss1 = case (ss0, ss1) of
      -- Rule (1)
      ((v := e) : s0, s1)    -> (v := e) `SSeq` go s0 s1
      -- Rule (2)
      ((SIf c st sf):s1, s2) -> SIf c (go (st:s1) s2) (go (sf:s1) s2)
      -- Rule (5) -- This has to go before the match for Rule (4) case 2
      ([SWhile c0 s0], [SWhile c1 s1]) ->
        let w = SWhile (c0 :&&: c1) (productProgram s0 s1)
            r = SIf c0 (SWhile c0 s0) (SIf c1 (SWhile c1 s1) SSkip)
        in w `SSeq` r
      -- Rule (4) (case 1)
      ((SWhile c0 s):s0, (SWhile c1 s'):s1) -> go [SWhile c0 s] [SWhile c1 s'] `SSeq` go s0 s1
      -- Rule (4) (case 2)
      (st@(SWhile _ _):s0, s1) -> go s1 (st:s0)
      -- Base case
      ([], s) -> sseq s
      _       -> error "The impossible happened, this case should not have happened"
