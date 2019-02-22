import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit

import Test.QuickCheck

import qualified ProgramRepTests as PRep
import qualified LogicTests as L

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "ProgramRep: Edits" PRep.unitPropertyBasedEdits
  , testGroup "ProgramRep: Expr printer" PRep.unitExprPrettyPrint
  , testGroup "Logic: Weakest Precondition" L.unitWP
  ]
