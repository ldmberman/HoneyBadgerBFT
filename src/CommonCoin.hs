{-# LANGUAGE ForeignFunctionInterface #-}

module CommonCoin (
    generateSecretShares
    ) where

import           Data.ByteString  (ByteString, packCString)

import           Foreign.C.String (CString (..))
import           Foreign.C.Types  (CUIntPtr (..))

foreign import ccall safe "generate_secret_shares"
    generate_secret_shares :: CUIntPtr -> IO CString


generateSecretShares :: Int -> IO ByteString
generateSecretShares n = generate_secret_shares (fromIntegral n) >>= \s -> packCString s

