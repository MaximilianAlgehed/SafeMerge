{-# LANGUAGE DataKinds #-}
module EmbeddedSyntax ( (==>)
                      , (&&&)
                      , notf
                      , (===)
                      , Num(..)
                      , IsString(..)
                      , return
                      , while
                      , (>>)
                      , skip
                      , hole
                      , ifThenElse
                      , (&))
                      where

import Logic
import ProgramRep

import Prelude hiding ((>>), fromInteger, return)
import GHC.Exts

{- Syntax for Formulas -}
(==>), (&&&) :: Formula -> Formula -> Formula
(==>) = (:->)
(&&&) = (:&)

notf :: Formula -> Formula
notf = FNot

(===) :: Expr -> Expr -> Formula
(===) = (:=:)

infix 4 ===
infixr 3 &&&
infixr 2 ==>

{- Syntax for conditions -}
(&) :: Condition -> Condition -> Condition
(&) = (:&&:)

infixr 3 &

{- Syntax for programs -}
hole :: Statement
hole = SHole

skip :: Statement
skip = SSkip

(>>) :: Statement -> Statement -> Statement
(>>) = SSeq

infixl >>

ifThenElse :: Condition -> Statement -> Statement -> Statement
ifThenElse = SIf

return :: a -> a
return = id

while :: Condition -> Statement -> Statement
while c s = SWhile c s
