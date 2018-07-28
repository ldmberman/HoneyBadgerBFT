{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.ByteString       (ByteString)
import qualified Data.Vector           as V

import           Control.Monad.Catch   (try)
import           Data.ByteString.Char8 (pack)

import qualified ErasureCoding         as EC

main :: IO ()
main =
    readInput >>= -- ask for a message and a coding scheme (N-f, f)
    encodeAndPrint >>= -- encode the message and output the N shards
    decodeAndPrint -- if successful, decode the message back from N-f shards

readInput :: IO (String, Int, Int)
readInput = do
    putStrLn "Enter a message: "
    message :: String <- getLine
    putStrLn "Enter a number of data shards (N - f):"
    dataShards :: Int <- readLn
    putStrLn "Enter a number of parity shards (f):"
    parityShards :: Int <- readLn

    return (message, dataShards, parityShards)

encodeAndPrint :: (String, Int, Int) -> IO (Either String ([ByteString], Int, Int))
encodeAndPrint (message, dataShards, parityShards) = do
    res <- try $ EC.encodeByteString (dataShards, parityShards) (pack message)
    case res of
        Left (EC.EncodingError msg) ->
           return $ Left msg
        Right encodedMessage        ->
           return $ Right (encodedMessage, dataShards, parityShards)

decodeAndPrint :: Either String ([ByteString], Int, Int) -> IO ()
decodeAndPrint (Left msg)       = putStrLn msg
decodeAndPrint (Right (encoded, d, s)) = do
    putStrLn $ "Encoded the message into: " ++ show encoded
    decodeAndPrint' (encoded, d, s)

decodeAndPrint' :: ([ByteString], Int, Int) -> IO ()
decodeAndPrint' (encoded, dataShards, parityShards) = do
    let partialShards = (take parityShards (repeat Nothing)) ++ (map Just (take (dataShards + 1) (drop parityShards encoded)))
    putStrLn $ "Decoding from: " ++ (show partialShards)
    maybeDecoded <- try $ EC.decodeByteString (dataShards, parityShards) partialShards
    case maybeDecoded of
        Left (EC.EncodingError msg) ->
            putStrLn msg
        Right message ->
            putStrLn $ "Decoded the message back: " ++ show message
