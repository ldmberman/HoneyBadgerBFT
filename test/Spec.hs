import           Test.Tasty (TestTree, defaultMain, testGroup)

import           RBCTest

tests :: TestTree
tests = testGroup "Tests" [RBCTest.unitTests]

main = defaultMain tests
