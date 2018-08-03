{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.ByteString       (ByteString)
import qualified Data.Vector           as V

import           Control.Exception     (handle)

import           Data.ByteString.Char8 (pack)

import           ErasureCoding         (EncodingError, decodeMatrix,
                                        decodeMessage, decodeShards,
                                        encodeByteString)
import           MerkleTree            (DecodingError, MerkleProof,
                                        mkMerkleProof, validateMerkleProof)

main :: IO ()
main =
    readInput >>= -- ask for a message and a coding scheme (N-f, f)
    encode >>= -- encode the message and output the N shards
    computeMerkleProofs >>= -- if successful, compute proofs of inclusion for the encoded parts composed into a Merkle tree
    decode >>= -- if successful, decode the message back from N-f shards
    verifyMerkleProofs -- if successful, verify proofs of inclusion for the decoded messages

readInput :: IO (String, Int, Int)
readInput = do
    putStrLn "Enter a message:"
    message :: String <- getLine
    putStrLn "\nEnter a number of data shards (N - f):"
    dataShards :: Int <- readLn
    putStrLn "\nEnter a number of parity shards (f):"
    parityShards :: Int <- readLn
    return (message, dataShards, parityShards)

encode :: (String, Int, Int) -> IO (Maybe ([ByteString], Int, Int, Int))
encode it = handle onEncodeFailure (encode' it)

onEncodeFailure :: EncodingError -> IO (Maybe ([ByteString], Int, Int, Int))
onEncodeFailure e = do
    putStrLn $ "\nEncoding failed: " ++ show e ++ "\n"
    return Nothing

encode' (message, dataShards, parityShards) =
    encodeByteString (dataShards, parityShards) (pack message) >>=
    \encodedMessage -> do
        putStrLn $ "\nEncoded: " ++ show encodedMessage ++ "\n"
        return $ Just (encodedMessage, length message, dataShards, parityShards)

computeMerkleProofs :: Maybe ([ByteString], Int, Int, Int) -> IO (Maybe ([ByteString], Int, Int, Int, [MerkleProof]))
computeMerkleProofs Nothing = return Nothing
computeMerkleProofs (Just (encoded, size, dataShards, parityShards)) = do
    let proofs = map (mkMerkleProof encoded) encoded
    showRoot proofs
    return $ Just (encoded, size, dataShards, parityShards, proofs)

showRoot []               = return ()
showRoot ((_, root, _):_) = putStrLn $ "Merkle tree root hash: " ++ show root ++ "\n"

decode :: Maybe ([ByteString], Int, Int, Int, [MerkleProof]) -> IO (Maybe ([ByteString], [MerkleProof]))
decode Nothing   = return Nothing
decode (Just it) = handle onDecodeFailure (decode' it)

onDecodeFailure :: EncodingError -> IO (Maybe ([ByteString], [MerkleProof]))
onDecodeFailure e = do
    putStrLn $ "Decoding failed: " ++ show e
    return Nothing

decode' (encoded, size, dataShards, parityShards, proofs) =
    let partialShards = (take parityShards (repeat Nothing)) ++ (map Just (take (dataShards + 1) (drop parityShards encoded))) in
        decodeMatrix (dataShards, parityShards) partialShards >>=
        \matrix -> do
            putStrLn $ "Decoding from: " ++ (show partialShards) ++ "\n"
            putStr $ "Decoded the message back: "
            decodeMessage' (size, dataShards, parityShards) matrix proofs

decodeMessage' (size, dataShards, parityShards) matrix proofs =
    decodeMessage (dataShards, parityShards) matrix size >>=
    \message -> do
        putStrLn $ show message ++ "\n"
        return $ Just (decodeShards matrix, proofs)

verifyMerkleProofs :: Maybe ([ByteString], [MerkleProof]) -> IO ()
verifyMerkleProofs Nothing = return ()
verifyMerkleProofs (Just (leafs, proofs)) = mapM_ verifyMerkleProofs' (zip leafs proofs)

verifyMerkleProofs' (leaf, proof) = do
    putStrLn $ "Verifying proof " ++ show proof ++ " for the leaf " ++ show leaf ++ "."
    verifyMerkleProof' proof

verifyMerkleProof' proof =
    handle onProofDecodeFailure (validateMerkleProof proof) >>=
    \proof -> do
        putStrLn $ "Successfully decoded the proof: " ++ show proof ++ "\n"
        return ()

onProofDecodeFailure :: DecodingError -> IO Bool
onProofDecodeFailure e = do
    putStrLn $ "Failed to decode the proof: " ++ show e ++ "\n"
    return False

