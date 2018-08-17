{-# LANGUAGE ScopedTypeVariables #-}
module RBCTest where

import           Control.Monad.Catch   (try)
import           Control.Monad.State   (runState)

import           Data.ByteString.Char8 (pack)
import           Data.Serialize        (encode)

import           RBC                   (In (..), Out (..), RBCError (..),
                                        receive, setup)

import           Test.Tasty            (testGroup)
import           Test.Tasty.HUnit      (assertEqual, assertFailure, testCase,
                                        (@?))

validator = pack "validator-1"
otherValidator = pack "validator-2"
oneValidator = [validator]
twoValidators = oneValidator ++ [otherValidator]
threeValidators = twoValidators ++ [pack "validator-3"]
fourValidators = threeValidators ++ [pack "validator-4"]

unitTests = testGroup "RBC unit tests"
    [ testCase "Fails to decode invalid message" $ testFailsToDecodeInvalidMessage
     ,testCase "Reports unknown validator" $ testReportsUnknownValidator
     ,testCase "Rejects its own message" $ testRejectsItsOwnMessage
     ,testCase "Broadcasts the input message" $ testBroadcastsInput ]

testFailsToDecodeInvalidMessage = do
    let (out, _) = runState (receive (pack "bad message")) (setup oneValidator)
    shouldbeError <- try out
    case shouldbeError of
        Right o -> assertFailure $ "Did not raise a decoding error, but returned: " ++ show o
        Left (InputDecodingError msg) -> return ()
        Left e -> assertFailure $ "Did not raise a decoding error, but raised: " ++ show e

testReportsUnknownValidator = do
    let (out, _) = runState (receive (encode (Input $ pack "A message", pack "unknown-validator"))) (setup fourValidators)
    shouldbeError <- try out
    case shouldbeError of
        Right o -> assertFailure $ "Did not report unknown validator, but returned: " ++ show o
        Left (UnknownValidator _) -> return ()
        Left e -> assertFailure $ "Did not report unknown validator, but raised: " ++ show e

testRejectsItsOwnMessage = do
    let (out, _) = runState (receive (encode (Input $ pack "A message", validator))) (setup oneValidator)
    shouldbeError <- try out
    case shouldbeError of
        Right o -> assertFailure $ "Did not reject its own message, but returned: " ++ show o
        Left (OwnMessageError _) -> return ()
        Left e -> assertFailure $ "Did not reject its own message, but raised: " ++ show e
testBroadcastsInput = do
    mapM_ testBroadcastsInput' [twoValidators, threeValidators, fourValidators]

testBroadcastsInput' validators = do
    let (out, _) = runState (receive (encode (Input $ pack "A message", otherValidator))) (setup validators)
    shouldbeBroadcast <- try out
    case shouldbeBroadcast of
        Right (Broadcast messages) -> assertBroadcastsInput messages validators (head validators)
        Right o -> assertFailure $ "Did not broadcast the expected message: " ++ show o
        Left (e :: RBCError) -> assertFailure $ "Raised an error " ++ show e

assertBroadcastsInput messages validators self = do
    assertEqual "Did not broadcast to all validators" (map (\(_, v) -> v) messages) (filter (\v -> v /= self) validators)
    mapM_ assertMessage messages where
        assertMessage (Val _, _) = return ()
        assertMessage msg = assertFailure $ "Broadcasted not `Val`: " ++ show msg

