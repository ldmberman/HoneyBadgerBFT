module CryptoTest where

import           Data.ByteString.Char8 (pack)

import           Test.Tasty            (testGroup)
import           Test.Tasty.HUnit      (assertEqual, testCase)

import           Crypto                (getPublicKey, signMessage,
                                        verifySignature)

unitTests = testGroup "Crypto utilities unit tests"
    [ testCase "Signs a message with a random secret key and verifies the signature" $ testSignMessageAndVerifySignature
     ,testCase "Fails to verify others signature" $ testFailsToVerifyOthersSignature ]

message = pack "A message to sign."
secretKeyA = (1, 2, 3, 4)
secretKeyB = (1, 1, 1, 1)

testSignMessageAndVerifySignature =
    signMessage message secretKeyA >>=
    \signature -> getPublicKey secretKeyA >>=
    \publicKey -> verifySignature signature message publicKey >>=
    \result -> assertEqual "Signature verification failed" True result

testFailsToVerifyOthersSignature =
    signMessage message secretKeyA >>=
    \signature -> getPublicKey secretKeyB >>=
    \publicKey -> verifySignature signature message publicKey >>=
    \result -> assertEqual "Signature verification succeeded" False result
