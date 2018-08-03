module ErasureCoding (
      EncodingError(..)
    , encodeByteString
    , decodeMatrix
    , decodeShards
    , decodeMessage
    ) where

import           Control.Monad.Catch             (Exception, MonadCatch,
                                                  MonadThrow, handle, throwM,
                                                  try)
import           Data.ByteString                 (ByteString, append, empty)

import           Data.Word                       (Word8)

import qualified Data.ReedSolomon                as RS
import qualified Data.Vector                     as V

import qualified Data.Vector.Storable            as SV

import           Data.Vector.Storable.ByteString (fromByteString, toByteString)


type ErasureScheme = (Int, Int)
type Message = ByteString
type EncodedShards = [ByteString]
type PartialShards = [Maybe ByteString]

data EncodingError = EncodingError String deriving Show

instance Exception EncodingError


encodeByteString :: (MonadThrow m, MonadCatch m) => ErasureScheme -> Message -> m EncodedShards
encodeByteString scheme message =
    newEncoder scheme >>=
    createShards message >>=
    encodeShards >>=
    matrixToShards

newEncoder :: (MonadThrow m, MonadCatch m) => ErasureScheme -> m RS.Encoder
newEncoder scheme = handle handleErrors (RS.new (fst scheme) (snd scheme))

createShards :: (MonadThrow m, MonadCatch m) => Message -> RS.Encoder -> m (RS.Matrix, RS.Encoder)
createShards message encoder = do
    maybeShards <- try $ RS.split encoder $ fromByteString message
    case maybeShards of
        Left e       -> handleErrors e
        Right shards -> return $ (shards, encoder)

encodeShards :: (MonadThrow m, MonadCatch m) => (RS.Matrix, RS.Encoder) -> m RS.Matrix
encodeShards (shards, encoder) = do
    maybeEncoded <- try $ (RS.encode encoder shards)
    case maybeEncoded of
        Left e        -> handleErrors e
        Right encoded -> return (shards V.++ encoded)

matrixToShards :: (MonadThrow m, MonadCatch m) => RS.Matrix -> m EncodedShards
matrixToShards matrix = return $ decodeShards matrix

-- Abstract the library errors away by raising a custom exception.
handleErrors RS.InvalidDataSize = throwM $ EncodingError "Invalid data size"
handleErrors RS.InvalidShardSize = throwM $ EncodingError "Invalid shard size"
handleErrors RS.EmptyShards = throwM $ EncodingError "Got empty shards :("
handleErrors (RS.InvalidNumberOfShards typ num) = throwM $ EncodingError $ "Invalid number of shards of type " ++ show typ ++ ", " ++ show num

decodeMatrix :: (MonadThrow m, MonadCatch m) => ErasureScheme -> PartialShards -> m RS.Matrix
decodeMatrix scheme shards =
    newEncoder scheme >>=
    reconstruct (deformat shards)

decodeMessage :: (MonadThrow m, MonadCatch m) => ErasureScheme -> RS.Matrix -> Int -> m Message
decodeMessage scheme matrix size = handle handleErrors (decodeMessage' scheme matrix size)

decodeMessage' scheme matrix size =
    newEncoder scheme >>=
    join matrix size

join matrix size encoder =
    RS.join encoder matrix size >>=
    \v -> return $ toByteString v

decodeShards :: RS.Matrix -> EncodedShards
decodeShards = V.toList . (V.map toByteString)

deformat shards = V.map (\v ->
    case v of
        Just value -> Just $ fromByteString value
        _          -> Nothing) (V.fromList shards)

reconstruct :: (MonadThrow m, MonadCatch m) => V.Vector (Maybe (SV.Vector Word8)) -> RS.Encoder -> m RS.Matrix
reconstruct shards encoder = handle handleErrors (RS.reconstruct encoder shards)
