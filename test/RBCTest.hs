{-# LANGUAGE ScopedTypeVariables #-}
module RBCTest where

import           Control.Monad.Catch   (try)
import           Control.Monad.State   (runState)

import           Data.ByteString.Char8 (pack)
import           Data.Serialize        (encode)

import           RBC                   (In (..), Out (..), RBCError (..),
                                        getEcho, receive, setup)

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
     ,testCase "Broadcasts the input message" $ testBroadcastsInput
     ,testCase "Echoes Val's" $ testEchoesVals
     ,testCase "Collects echoes" $ testCollectsEchoes
     ,testCase "Fails to parse improperly built proofs" testFailsToParseImproperlyBuiltProofs
     ,testCase "Ignores invalid proofs" $ testIgnoresInvalidProofs
     ,testCase "Collects messages from distinct validators" $ testCollectsMessagesFromDistinctValidators ]

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
    assertEqual "Did not broadcast to all validators" (filter (\v -> v /= self) validators) (map (\(_, v) -> v) messages)
    mapM_ assertMessage messages where
        assertMessage (Val _, _) = return ()
        assertMessage msg = assertFailure $ "Broadcasted not `Val`: " ++ show msg

dummyProof = (pack "proof", pack "root", pack "leaf")
testEchoesVals = do
    let (out, _) = runState (receive (encode (Val $ dummyProof, otherValidator))) (setup fourValidators)
    (Broadcast messages) <- out
    assertEqual "Did not broadcast to all validators" (filter (\v -> v /= validator) fourValidators) (map (\(_, v) -> v) messages)
    mapM_ assertMessage messages where
        assertMessage (Echo msg, _) = assertEqual "Echoed message is different" dummyProof msg
        assertMessage msg = assertFailure $ "Did not echoed the message, but returned: " ++ show msg

-- A proof for the part "A m" of the message "A message" in the (4, 2) scheme.
validProof = (pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@606e56790fadcc19aa25a5b22973a22b33afcc1ed08c0f4c67444471792e25ee\NUL\NUL\NUL\NUL\NUL\NUL\NUL@dd097c371ab05b5656ba2180136012592fc0817a5b3b05f226c3d081941e3ce2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL@2699550d5527e80c42e861c33c196319d64fd74b10fb48c2eb7f97519056b984\NUL\NUL\NUL\NUL\NUL\NUL\NUL@50946fbcc217f6940d18e68e7060a974c111b361be5dfb5445ffc82772ce0b6b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL@a47aa557637002bb55c9a19a678d3f83c1baf6e994fd890dfc7267fd0b62effb\NUL\NUL\NUL\NUL\NUL\NUL\NUL@bbcaef7a54b930e94bbd24b2ee5a573b8063d110c2906daa51c7b4f2e8d2f362\NUL", pack "76b7b26d1c6f4ff4df64cb20f30ff8e1e538fd791ad16daf577fa9017f050dda", pack "A m")
invalidProof = let (p, root, _) = validProof in (p, root, pack "essa")

testCollectsEchoes = do
    let (out, _) = runState (receive (encode (Echo $ validProof, otherValidator))) (setup fourValidators)
    maybeResult <- try out
    case maybeResult of
        Left (e :: RBCError) -> assertFailure $ "Failed to process the echo message: " ++ show e
        Right (StoreEcho (proof, validator)) -> assertEqual "The echo changed during processing" validProof proof
        Right o -> assertFailure $ "Did not output the correct echo processing result: " ++ show o

testCollectsMessagesFromDistinctValidators = do
    let initialState = (setup fourValidators) {
        getEcho = [(otherValidator, validProof)]
    }
    let (out, _) = runState (receive (encode (Echo $ validProof, otherValidator))) initialState
    maybeResult <- try out
    case maybeResult of
        Left (e :: RBCError) -> assertFailure $ "Failed to process the echo message: " ++ show e
        Right None -> return ()
        Right o -> assertFailure $ "Did not ignore the repeated message but returned: " ++ show o

testFailsToParseImproperlyBuiltProofs = do
    let (out, _) = runState (receive (encode (Echo $ dummyProof, otherValidator))) (setup fourValidators)
    maybeResult <- try out
    case maybeResult of
        Left (e :: RBCError) -> return ()
        Right o -> assertFailure $ "Did not complain about the improperly coded proof but returned: " ++ show o

testIgnoresInvalidProofs = do
    let (out, _) = runState (receive (encode (Echo $ invalidProof, otherValidator))) (setup fourValidators)
    maybeResult <- try out
    case maybeResult of
        Left (e :: RBCError) -> assertFailure $ "Failed to process the echo message: " ++ show e
        Right None -> return ()
        Right o -> assertFailure $ "Did not ignore invalid proof but returned: " ++ show o
