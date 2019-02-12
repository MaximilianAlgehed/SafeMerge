module Verification where

import Logic
import ProgramRep

import qualified Data.Set as S

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
      pre = foldr (:&) (0 :=: 0) [ ((Var . Name $ v ++ "_0") :=: (Var . Name $ v ++ "_1")) :&
                                   (((Var . Name $ v ++ "_1") :=: (Var . Name $ v ++ "_2")) :&
                                    ((Var . Name $ v ++ "_2") :=: (Var . Name $ v ++ "_3")))
                                 | Name v <- S.toList vs ]
      x1 o = (FNot ((Var . Name $ o ++ "_0") :=: (Var . Name $ o ++ "_1")))
           :-> ((Var . Name $ o ++ "_1") :=: (Var . Name $ o ++ "_3"))
      x2 o = (FNot ((Var . Name $ o ++ "_0") :=: (Var . Name $ o ++ "_2")))
           :-> ((Var . Name $ o ++ "_2") :=: (Var . Name $ o ++ "_3"))
      x3 o =  ((Var . Name $ o ++ "_0") :=: (Var . Name $ o ++ "_1"))
           :& (((Var . Name $ o ++ "_1") :=: (Var . Name $ o ++ "_2"))
           :& ((Var . Name $ o ++ "_2") :=: (Var . Name $ o ++ "_3")))
      post = foldr (:&) (0 :=: 0) [ (x1 o :& x2 o) :| x3 o | Name o <- outputs ]
  return $ Hoare pre (productProgram (productProgram po pa) (productProgram pb pm)) post
