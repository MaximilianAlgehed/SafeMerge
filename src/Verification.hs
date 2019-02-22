{-# LANGUAGE OverloadedStrings #-}
module Verification where

import Logic
import ProgramRep

import qualified Data.Set as S
import Data.SBV hiding ((#))
import Data.SBV.Control hiding (Name)
import qualified Data.Map as M
import Control.Monad.Trans
import Data.List

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

exprToSInteger :: M.Map Variable SInteger -> Expr -> Maybe SInteger
exprToSInteger env = go
  where
    go e = case e of
      Var v     -> M.lookup v env
      Lit i     -> return . literal $ toInteger i
      e0 :+: e1 -> (+) <$> go e0 <*> go e1
      e0 :-: e1 -> (-) <$> go e0 <*> go e1

formulaToSBool :: M.Map Variable SInteger -> Formula -> Maybe SBool
formulaToSBool env = go
  where
    go f = case f of
      e0 :=: e1 -> (.==) <$> exprToSInteger env e0 <*> exprToSInteger env e1
      e0 :>  e1 -> (.>)  <$> exprToSInteger env e0 <*> exprToSInteger env e1
      f0 :&  f1 -> (.&&) <$> go f0 <*> go f1
      f0 :|  f1 -> (.||) <$> go f0 <*> go f1
      f0 :-> f1 -> (.=>) <$> go f0 <*> go f1
      FNot f'   -> sNot  <$> go f'

vc :: HoareTriple -> Maybe Formula
vc h = do
  weakestPrecondition <- wp (program h) (postcondition h)
  return $ precondition h :-> weakestPrecondition

verify :: HoareTriple -> IO ()
verify h = runSMT $ do
  case vc h of
    Nothing -> lift $ putStrLn "Error!"
    Just f  -> do
      let vs = S.toList $ formulaVars f
      svs <- sequence [ free n | Name n <- vs ]
      let env = M.fromList $ zip vs svs
      case formulaToSBool env f of
        Nothing -> lift $ putStrLn "Error!"
        Just sb -> do
          constrain $ sNot sb
          query $ do
            cs <- checkSat
            case cs of
              Unk   -> lift $ putStrLn "Error!"
              Unsat -> lift $ putStrLn "Verified!"
              Sat   -> do
                vals <- sequence [ getValue sv | sv <- svs ]
                lift . putStrLn $ "Counterexample: {" ++ intercalate ", " [ n ++ " := " ++ show v | (Name n, v) <- zip vs vals ] ++ "}"

programEquivalence :: Statement -> Statement -> [Variable] -> HoareTriple
programEquivalence p0 p1 outputs =
  let p0'  = setVariableIndex p0 0
      p1'  = setVariableIndex p1 1
      vs   = vars p0 `S.union` vars p1
      -- Given initial states
      pre  = foldr (:&) (0 :=: 0) [ Var (v # 0) :=: Var (v # 1) | v <- S.toList vs ]
      -- All the outputs are the same
      post = foldr (:&) (0 :=: 0) [ Var (v # 0) :=: Var (v # 1) | v <- outputs ]
  in Hoare pre (productProgram p0' p1') post

mergeCandidateHoareTriple :: Statement         -- Contains holes
                          -> Edit              -- Base program
                          -> Edit              -- Branch A
                          -> Edit              -- Branch B
                          -> Edit              -- Merge candidate
                          -> [Variable]        -- Output variables
                          -> Maybe HoareTriple -- Resulting program to verify
mergeCandidateHoareTriple s deltaO deltaA deltaB deltaM outputs = do
  -- Generate the programs
  po_ <- applyEdit s deltaO
  let po = setVariableIndex po_ 0
  pa_ <- applyEdit s deltaA 
  let pa = setVariableIndex pa_ 1
  pb_ <- applyEdit s deltaB 
  let pb = setVariableIndex pb_ 2
  pm_ <- applyEdit s deltaM
  let pm = setVariableIndex pm_ 3
  -- Generate the precondition
  let vs = vars po_ `S.union` vars pa_ `S.union` vars pb_ `S.union` vars pm_
      pre = foldr (:&) (0 :=: 0) [ (Var (v # 0) :=: Var (v # 1)) :&
                                   ((Var (v # 1) :=: Var (v # 2)) :&
                                    (Var (v # 2) :=: Var (v # 3)))
                                 | v <- S.toList vs ]
      x1 o = (FNot $ Var (o # 0) :=: Var (o # 1))
           :-> (Var (o # 1) :=: Var (o # 3))
      x2 o = (FNot $ Var (o # 0) :=: Var (o # 2))
           :-> (Var (o # 2) :=: Var (o # 3))
      x3 o = (Var (o # 0) :=: Var (o # 1))
           :& ((Var (o # 1) :=: Var (o # 2))
           :& (Var (o # 2) :=: Var (o # 3)))
      post = foldr (:&) (0 :=: 0) [ (x1 o :& x2 o) :| x3 o | o <- outputs ]
  return $ Hoare pre (productProgram (productProgram po pa) (productProgram pb pm)) post
