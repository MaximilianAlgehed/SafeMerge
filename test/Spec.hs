import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import ProgramRepTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "ProgramRep" [
    testProperty "prop_id" prop_id,
    testProperty "prop_apply_count" prop_apply_count
    ]
  ]
