import           Test.Tasty     (TestTree, defaultMain, testGroup)

import           CommonCoinTest
import           RBCTest

tests :: TestTree
tests = testGroup "Tests" [RBCTest.unitTests, CommonCoinTest.unitTests]

main = defaultMain tests
