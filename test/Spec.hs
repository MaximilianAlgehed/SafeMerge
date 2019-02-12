import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit

import Test.QuickCheck

import qualified ProgramRepTests as PRep

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "ProgramRep: Edits" PRep.unitPropertyBasedEdits
  , testGroup "ProgramRep: Expr printer" PRep.unitExprPrettyPrint
  ]
