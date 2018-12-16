{-# LANGUAGE ForeignFunctionInterface #-}

module Crypto (
     signMessage
    ,verifySignature
    ,getPublicKey
    ) where

import           Data.ByteString       (ByteString, pack, unpack)
import qualified Data.ByteString       as BS
import           Data.Int              (Int64)

import           Data.Word             (Word8)

import           Foreign.C.Types       (CSize (..), CULong (..))
import           Foreign.Marshal.Alloc (free, malloc)
import           Foreign.Marshal.Array (newArray, peekArray)
import           Foreign.Marshal.Utils (new)
import           Foreign.Ptr           (Ptr, plusPtr)
import           Foreign.Storable      (peek)

foreign import ccall unsafe "sign"
    sign :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> CULong -> CULong -> CULong -> CULong -> IO CSize

foreign import ccall unsafe "public_key_from_secret_key"
    public_key_from_secret_key :: CULong -> CULong -> CULong -> CULong -> Ptr (Ptr Word8) -> IO CSize

foreign import ccall unsafe "verify"
    verify :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO Bool

foreign import ccall unsafe "arr_free"
    arr_free :: Ptr Word8 -> CSize -> IO ()

type SecretKey = (Int64, Int64, Int64, Int64)
type PublicKey = ByteString

signMessage :: ByteString -> SecretKey -> IO ByteString
signMessage message secretKey =
    (newArray $ unpack message) >>= signMessage' secretKey (BS.length message)

signMessage' secretKey msgSize msgPtr =
    (malloc :: IO (Ptr Word8)) >>= -- Allocate memory for the signature.
    \externalPtr -> new externalPtr >>=
    signMessage'' secretKey msgPtr msgSize externalPtr

signMessage'' (fr1, fr2, fr3, fr4) msgPtr msgSize externalPtr sigPtrPtr =
    sign msgPtr
         (fromIntegral msgSize)
         sigPtrPtr
         (fromIntegral fr1)
         (fromIntegral fr2)
         (fromIntegral fr3)
         (fromIntegral fr4) >>=
    \sigSize -> peek sigPtrPtr >>=
    peekArray (fromIntegral sigSize) >>=
    cleanupAfterSign externalPtr sigSize sigPtrPtr msgPtr

cleanupAfterSign externalPtr sigSize sigPtrPtr msgPtr signedMessage =
    free sigPtrPtr >> -- We allocated the pointer to the byte array - free it.
    arr_free externalPtr sigSize >> -- The other side allocated the byte array - request to free it.
    free msgPtr >> -- The message array is also allocated by us.
    (return $ pack signedMessage)

getPublicKey :: SecretKey -> IO ByteString
getPublicKey secretKey =
    (malloc :: IO (Ptr Word8)) >>= -- Allocate memory for the public key.
    \externalPtr -> new externalPtr >>=
    getPublicKey' secretKey externalPtr

getPublicKey' (fr1, fr2, fr3, fr4) externalPtr pkPtrPtr =
    public_key_from_secret_key (fromIntegral fr1)
                               (fromIntegral fr2)
                               (fromIntegral fr3)
                               (fromIntegral fr4)
                               pkPtrPtr >>=
    \pkSize -> peek pkPtrPtr >>=
    peekArray (fromIntegral pkSize) >>=
    cleanupAfterGetPublicKey externalPtr pkSize pkPtrPtr

cleanupAfterGetPublicKey externalPtr pkSize pkPtrPtr publicKey =
    arr_free externalPtr pkSize >> -- The other side allocated the byte array - request to free it.
    free pkPtrPtr >> -- We allocated the pointer to the byte array - free it.
    (return $ pack publicKey)

verifySignature :: ByteString -> ByteString -> PublicKey -> IO Bool
verifySignature signature message publicKey =
    (newArray $ unpack (BS.append signature $ BS.append message publicKey)) >>=
    verifySignature' (BS.length signature) (BS.length message) (BS.length publicKey)

verifySignature' sigSize msgSize pkSize sigPtr =
    verify msgPtr (fromIntegral msgSize) sigPtr (fromIntegral sigSize) pkPtr (fromIntegral pkSize) >>=
    verifySignature'' sigPtr
        where msgPtr = sigPtr `plusPtr` sigSize
              pkPtr = msgPtr `plusPtr` msgSize

verifySignature'' sigPtr isValid =
    free sigPtr >>
    return isValid
