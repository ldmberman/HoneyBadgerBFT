module CommonCoinTest where

import           Test.Tasty       (testGroup)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import           CommonCoin       (generateSecretShares)


unitTests = testGroup "Common Coin unit tests"
    [ testCase "Generates a public key and secret shares" $ testGenerateSecretShares ]

testGenerateSecretShares = do
    share <- generateSecretShares 4
    putStrLn $ show share
