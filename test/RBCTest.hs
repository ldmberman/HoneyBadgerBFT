{-# LANGUAGE ScopedTypeVariables #-}
module RBCTest where

import           Control.Monad.Catch   (try)
import           Control.Monad.State   (runState)

import           Data.ByteString.Char8 (pack)
import           Data.Serialize        (encode)

import           RBC                   (In (..), Out (..), RBCError (..),
                                        getEcho, receive, setup, storeEcho)

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
     ,testCase "Collects messages from distinct validators" $ testCollectsMessagesFromDistinctValidators
     ,testCase "Stores echoes" $ testStoresEchoes ]

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
    broadcast <- out
    case broadcast of
        Broadcast messages -> assertBroadcastsInput messages validators (head validators)
        o -> assertFailure $ "Did not broadcast the expected message: " ++ show o

assertBroadcastsInput messages validators self = do
    assertEqual "Did not broadcast to all validators" (filter (\v -> v /= self) validators) (map (\(_, v) -> v) messages)
    mapM_ assertMessage messages where
        assertMessage (Val _, _) = return ()
        assertMessage msg = assertFailure $ "Broadcasted not `Val`: " ++ show msg

dummyProof = (pack "proof", pack "root", pack "leaf")
testEchoesVals = do
    let (out, _) = runState (receive (encode (Val $ dummyProof, otherValidator))) (setup fourValidators)
    Broadcast messages <- out
    assertEqual "Did not broadcast to all validators" (filter (\v -> v /= validator) fourValidators) (map (\(_, v) -> v) messages)
    mapM_ assertMessage messages where
        assertMessage (Echo msg, _) = assertEqual "Echoed message is different" dummyProof msg
        assertMessage msg = assertFailure $ "Did not echoed the message, but returned: " ++ show msg

validRoot = pack "d6bc9d2f743a933cb807e4d97fdcbbad2e032ade5df5242a308af4a11b12fb2e"
-- A proof for the part "A mes" of the message "A message" in the (2, 2) scheme.
validProof = (pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@6eeb07289f281c2ff6fd02578c4398ca7fbaf610e27f18f248991280a12af55d\NUL\NUL\NUL\NUL\NUL\NUL\NUL@b9c383ba4dd754a6cf9443a9b85cbd801c93b402378ee46f304ea3a740de3f75\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f93f2238d46169c74e3e42840dd97da4b37ef06ad1b58446606b0df8c507606c\SOH", validRoot, pack "A mes")
invalidProof = let (p, root, _) = validProof in (p, root, pack "essa")
-- Same message, first parity part.
secondValidProof = (pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f22b572c5f0fceb4baa513d03d9875aba0d257bb937b625b215409990457028c\NUL\NUL\NUL\NUL\NUL\NUL\NUL@aeb6f7984270bc4ddd4e51c9759740c24c6d0a64f5c1c5055117f1b105658db4\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f93f2238d46169c74e3e42840dd97da4b37ef06ad1b58446606b0df8c507606c\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\NUL", pack "d6bc9d2f743a933cb807e4d97fdcbbad2e032ade5df5242a308af4a11b12fb2e", pack "%\162ye\149")
-- Same message, second parity part.
thirdValidProof = (pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@aeb6f7984270bc4ddd4e51c9759740c24c6d0a64f5c1c5055117f1b105658db4\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f22b572c5f0fceb4baa513d03d9875aba0d257bb937b625b215409990457028c\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f93f2238d46169c74e3e42840dd97da4b37ef06ad1b58446606b0df8c507606c\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\NUL", pack "d6bc9d2f743a933cb807e4d97fdcbbad2e032ade5df5242a308af4a11b12fb2e", pack "\ETB\227se\230")

testCollectsEchoes = do
    let (out, _) = runState (receive (encode (Echo $ validProof, otherValidator))) (setup fourValidators)
    result <- out
    case result of
        StoreEcho (proof, validator) -> assertEqual "The echo changed during processing" validProof proof
        o -> assertFailure $ "Did not output the correct echo processing result: " ++ show o

testCollectsMessagesFromDistinctValidators = do
    let initialState = (setup fourValidators) {
        getEcho = [(otherValidator, validProof)]
    }
    let (out, _) = runState (receive (encode (Echo $ validProof, otherValidator))) initialState
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not ignore the repeated message but returned: " ++ show o

testFailsToParseImproperlyBuiltProofs = do
    let (out, _) = runState (receive (encode (Echo $ dummyProof, otherValidator))) (setup fourValidators)
    maybeResult <- try out
    case maybeResult of
        Left (e :: RBCError) -> return ()
        Right o -> assertFailure $ "Did not complain about the improperly coded proof but returned: " ++ show o

testIgnoresInvalidProofs = do
    let (out, _) = runState (receive (encode (Echo $ invalidProof, otherValidator))) (setup fourValidators)
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not ignore invalid proof but returned: " ++ show o

testStoresEchoes = do
    let (out, stateAfterOneEcho) = runState (storeEcho validProof $ fourValidators !! 1) (setup fourValidators)
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not just store the echo but returned: " ++ show o

    let (out, stateAfterTwoEchoes) = runState (storeEcho secondValidProof $ fourValidators !! 2) stateAfterOneEcho
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not just store the echo but returned: " ++ show o

    let (out, stateAfterThreeEchoes) = runState (storeEcho thirdValidProof $ fourValidators !! 3) stateAfterTwoEchoes
    result <- out
    case result of
        Broadcast messages -> assertEqual "Did not broadcast the correct messages" [(Ready validRoot, v) | v <- fourValidators, v /= validator] messages
        o -> assertFailure $ "Did not command to interpolate but returned: " ++ show o

    assertEqual "Did not store all echoes correctly" (zip (drop 1 fourValidators) [validProof, secondValidProof, thirdValidProof]) (getEcho stateAfterThreeEchoes)
