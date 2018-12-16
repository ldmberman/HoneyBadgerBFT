import           Test.Tasty     (TestTree, defaultMain, testGroup)

import           CommonCoinTest
import           CryptoTest
import           RBCTest

tests :: TestTree
tests = testGroup "Tests" [RBCTest.unitTests, CommonCoinTest.unitTests, CryptoTest.unitTests]

main = defaultMain tests
