{-# LANGUAGE ScopedTypeVariables #-}
module RBCTest where

import           Control.Monad.Catch   (MonadMask, try)
import           Control.Monad.State   (runState)

import           Data.ByteString.Char8 (pack)

import           RBC                   (In (..), Out (..), RBCError (..),
                                        RBCState (..), markReadySent, receive,
                                        setup, storeEcho, storeMessage,
                                        storeReady)

import           Test.Tasty            (testGroup)
import           Test.Tasty.HUnit      (assertEqual, assertFailure, testCase)


unitTests = testGroup "RBC unit tests"
    [ testCase "Reports unknown validator" $ testReportsUnknownValidator
     ,testCase "Rejects its own message" $ testRejectsItsOwnMessage
     ,testCase "Broadcasts the input message" $ testBroadcastsInput
     ,testCase "Echoes Val's" $ testEchoesVals
     ,testCase "Collects echoes" $ testCollectsEchoes
     ,testCase "Fails to parse improperly built proofs" testFailsToParseImproperlyBuiltProofs
     ,testCase "Ignores invalid proofs" $ testIgnoresInvalidProofs
     ,testCase "Collects messages from distinct validators" $ testCollectsMessagesFromDistinctValidators
     ,testCase "Stores echoes" $ testStoresEchoes
     ,testCase "Does not re-broadcast Ready" $ testDoesNotReBroadcastReady
     ,testCase "Broadcasts Ready after receiving matching Ready" $ testBroadcastsReadyAfterReceivingMatchingReady
     ,testCase "Does not broadcast mismatching Ready" $ testDoesNotBroadcastMismatchingReady
     ,testCase "Outputs the message after the sufficient number of inputs" $ testOutputsAfterSufficientInput ]

dummyProof = ((pack "proof", pack "root", pack "leaf"), 1)
validRoot = pack "71d7596632e783faf2d375e8ad952db1c73a94eae342df189b15b747a0915816"
-- A proof for the part "A mes" of the message "A message" in the (2, 2) scheme.
validProof = ((pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@6eeb07289f281c2ff6fd02578c4398ca7fbaf610e27f18f248991280a12af55d\NUL\NUL\NUL\NUL\NUL\NUL\NUL@b9c383ba4dd754a6cf9443a9b85cbd801c93b402378ee46f304ea3a740de3f75\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\NUL\NUL\NUL\NUL\NUL\NUL\NUL@5b304a7122376d5aac29130957632c814242250fb6e99042b89b9e8243bf1e26\NUL", validRoot, pack "A mes"), 9)
-- Same message, second data part.
secondValidProof = ((pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@b9c383ba4dd754a6cf9443a9b85cbd801c93b402378ee46f304ea3a740de3f75\NUL\NUL\NUL\NUL\NUL\NUL\NUL@6eeb07289f281c2ff6fd02578c4398ca7fbaf610e27f18f248991280a12af55d\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\NUL\NUL\NUL\NUL\NUL\NUL\NUL@5b304a7122376d5aac29130957632c814242250fb6e99042b89b9e8243bf1e26\NUL", validRoot, pack "sage\NUL"), 9)
-- Same message, first parity part.
thirdValidProof = ((pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f22b572c5f0fceb4baa513d03d9875aba0d257bb937b625b215409990457028c\NUL\NUL\NUL\NUL\NUL\NUL\NUL@aeb6f7984270bc4ddd4e51c9759740c24c6d0a64f5c1c5055117f1b105658db4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL@5b304a7122376d5aac29130957632c814242250fb6e99042b89b9e8243bf1e26\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\SOH", validRoot, pack "%\162ye\149"), 9)
-- Same message, second parity part.
fourthValidProof = ((pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL@aeb6f7984270bc4ddd4e51c9759740c24c6d0a64f5c1c5055117f1b105658db4\NUL\NUL\NUL\NUL\NUL\NUL\NUL@f22b572c5f0fceb4baa513d03d9875aba0d257bb937b625b215409990457028c\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL@5b304a7122376d5aac29130957632c814242250fb6e99042b89b9e8243bf1e26\NUL\NUL\NUL\NUL\NUL\NUL\NUL@7176d01dbadd052dac9865602ca2279ac3f11cc83341b1a0c1fbd00fc54dfd96\SOH", validRoot, pack "\ETB\227se\230"), 9)
validProofs = [validProof, secondValidProof, thirdValidProof, fourthValidProof]
invalidProof = let ((p, root, _), size) = validProof in ((p, root, pack "sage\NUL"), size)

testReportsUnknownValidator = do
    let (out, _) = runState (receive (Input $ pack "A message") 5) (setup 4 0)
    shouldbeError <- try out
    case shouldbeError of
        Right o -> assertFailure $ "Did not report unknown validator, but returned: " ++ show o
        Left (UnknownValidator _) -> return ()
        Left e -> assertFailure $ "Did not report unknown validator, but raised: " ++ show e

testRejectsItsOwnMessage = do
    let (out, _) = runState (receive (Input $ pack "A message") 0) (setup 1 0)
    shouldbeError <- try out
    case shouldbeError of
        Right o -> assertFailure $ "Did not reject its own message, but returned: " ++ show o
        Left (OwnMessageError _) -> return ()
        Left e -> assertFailure $ "Did not reject its own message, but raised: " ++ show e

testBroadcastsInput = do
    mapM_ testBroadcastsInput' [(2, 0), (2, 1), (3, 0), (3, 1), (3, 2), (4, 0), (4, 1), (4, 2)]

testBroadcastsInput' (n, self) = do
    let (out, _) = runState (receive (Input $ pack "A message") (head [v | v <- [0..n-1], v /= self])) (setup n self)
    broadcast <- out
    case broadcast of
        Broadcast messages -> assertBroadcastsInput messages n self
        o -> assertFailure $ "Did not broadcast the expected message: " ++ show o

assertBroadcastsInput messages n self = do
    assertEqual "Did not broadcast to all validators" [v | v <- [0..n-1], v /= self] (map (\(_, v) -> v) messages)
    mapM_ assertMessage messages where
        assertMessage (Val (_, size), _) = assertEqual "Did not broadcast the correct message size: " 9 size
        assertMessage msg = assertFailure $ "Broadcasted not `Val`: " ++ show msg

testEchoesVals = do
    let (out, _) = runState (receive (Val dummyProof) 1) (setup 4 0)
    Broadcast messages <- out
    assertEqual "Did not broadcast correct messages to all validators" (map (\v -> (Echo dummyProof, v)) [1, 2, 3]) messages

testCollectsEchoes = do
    let (out, _) = runState (receive (Echo secondValidProof) 1) (setup 4 0)
    result <- out
    case result of
        StoreEcho (proof, validator) -> assertEqual "The echo changed during processing" secondValidProof proof
        o -> assertFailure $ "Did not output the correct echo processing result: " ++ show o

testCollectsMessagesFromDistinctValidators = do
    let initialState = (setup 4 0) {
        getEcho = [(1, secondValidProof)]
    }
    let (out, _) = runState (receive (Echo secondValidProof) 1) initialState
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not ignore the repeated message but returned: " ++ show o

testFailsToParseImproperlyBuiltProofs = do
    let (out, _) = runState (receive (Echo dummyProof) 1) (setup 4 0)
    maybeResult <- try out
    case maybeResult of
        Left (e :: RBCError) -> return ()
        Right o -> assertFailure $ "Did not complain about the improperly coded proof but returned: " ++ show o

testIgnoresInvalidProofs = do
    let (out, _) = runState (receive (Echo invalidProof) 1) (setup 4 0)
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not ignore invalid proof but returned: " ++ show o

testStoresEchoes = do
    let (out, stateAfterOneEcho) = runState (storeEcho secondValidProof 1) (setup 4 0)
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not just store the echo but returned: " ++ show o

    let (out, stateAfterTwoEchoes) = runState (storeEcho thirdValidProof 2) stateAfterOneEcho
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not just store the echo but returned: " ++ show o

    let (out, stateAfterThreeEchoes) = runState (storeEcho fourthValidProof 3) stateAfterTwoEchoes
    result <- out
    case result of
        StoreMessage (message, Broadcast messages) ->
            assertEqual "Did not broadcast the correct messages" [(Ready validRoot, v) | v <- [1, 2, 3]] messages >>
            assertEqual "Did not record the correct message" (pack "A message") message
        o -> assertFailure $ "Did not interpolate shards but returned: " ++ show o

    assertEqual "Did not store all echoes correctly" (zip [1, 2, 3] [secondValidProof, thirdValidProof, fourthValidProof]) (getEcho stateAfterThreeEchoes)

buildStateWithTwoEchoes = do
    let (out, s) = runState (storeEcho secondValidProof 1) (setup 4 0)
    _ <- out
    let (out, state) = runState (storeEcho thirdValidProof 2) s
    _ <- out
    return state

testDoesNotReBroadcastReady = do
    state <- buildStateWithTwoEchoes

    let (_, stateAfterReadySent) = runState markReadySent state
    let (maybeOut, _) = runState (storeEcho fourthValidProof 3) stateAfterReadySent
    out <- maybeOut
    case out of
        None -> return ()
        _ -> assertFailure $ "Did not ignore yet another Ready but returned: " ++ show out

testBroadcastsReadyAfterReceivingMatchingReady = do
    let (out, stateAfterOneReady) = runState (storeReady (pack "A root") 1) (setup 4 0)
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not just store Ready but returned: " ++ show o

    let (out, stateAfterTwoReadies) = runState (storeReady (pack "A root") 2) stateAfterOneReady
    result <- out
    case result of
        Broadcast messages -> assertEqual "Did not broadcast the correct messages" (map (\v -> (Ready (pack "A root"), v)) [1, 2, 3]) messages
        o -> assertFailure $ "Did not broadcast messages but returned:  " ++ show o

    let (_, afterReadySent) = runState markReadySent stateAfterTwoReadies
    let (out, _) = runState (storeReady (pack "A root") 3) afterReadySent
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not ignore yet another Ready but returned: " ++ show o

testDoesNotBroadcastMismatchingReady = do
    let root = pack "A root"
    let mismatchingRoot = pack "A mismatching root"

    let state = (setup 4 0) {
        getReady = [
             (1, root)
            ,(2, root)
            ,(1, mismatchingRoot)
        ]
    }
    let (out, _) = runState (storeReady mismatchingRoot 2) state
    result <- out
    case result of
        None -> return ()
        o -> assertFailure $ "Did not ignore mismatching Ready but returned: " ++ show o

testOutputsAfterSufficientInput = do
    -- The output is expected after at least 2f + 1 Ready and at least N - 2f Echo.

    -- enough of Ready but not enough of Echo
    let state = (setup 4 0) {
         getEcho = []
        ,getReady = [
             (1, validRoot)
            ,(2, validRoot)
            ,(3, validRoot)
        ]
        ,getReadySent = True
    }
    let (out, _) = runState (storeEcho secondValidProof 1) state
    result <- out
    case result of
        None -> return ()
        o    -> assertFailure $ "Returned unexpected output: " ++ show o

    let (out, _) = runState (storeReady validRoot 1) state
    result <- out
    case result of
        None -> return ()
        o    -> assertFailure $ "Returned unexpected output: " ++ show o

    -- enough of Echo but not enough of Ready
    let state = (setup 4 0) {
         getEcho = [
            (1, secondValidProof)
         ]
        ,getReady = [
            (1, validRoot)
        ]
        ,getReadySent = True
    }
    let (out, _) = runState (storeEcho thirdValidProof 2) state
    result <- out
    case result of
        None -> return ()
        o    -> assertFailure $ "Returned unexpected output: " ++ show o

    let (out, _) = runState (storeReady validRoot 2) state
    result <- out
    case result of
        None -> return ()
        o    -> assertFailure $ "Returned unexpected output: " ++ show o

    -- enough of Echo and Ready
    let state = (setup 4 0) {
         getEcho = [
             (1, secondValidProof)
            ,(2, thirdValidProof)
         ]
        ,getReady = [
              (1, validRoot)
             ,(2, validRoot)
             ,(3, validRoot)
        ]
        ,getReadySent = True
    }
    let (_, withMessage) = runState (storeMessage $ pack "A message") state
    let (out, _) = runState (storeEcho fourthValidProof 3) withMessage
    result <- out
    case result of
        Output msg -> assertEqual "Did not output the correct message" (pack "A message") msg
        o    -> assertFailure $ "Did not output the message after storing echo but returned: " ++ show o

    let (out, _) = runState (storeReady validRoot 2) withMessage
    result <- out
    case result of
        Output msg -> assertEqual "Did not output the correct message" (pack "A message") msg
        o    -> assertFailure $ "Did not output the message after storing Ready but returned: " ++ show o
