{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}

module RBC where

-- Reliable Broadcast
--
-- For more details, see the article - https://eprint.iacr.org/2016/199.pdf
--
-- • upon input(v) (if Pi = PSender):
--   let{sj} j∈[N] be the blocks of an (N − 2f, N) erasure coding scheme applied to v
--   let h be a Merkle tree root computed over {sj}
--   send VAL(h,bj,sj) to each party Pj, where bj is the jth Merkle tree branch
-- • upon receiving VAL(h,bi,si) from PSender, multicast ECHO(h,bi,si)
-- • upon receiving ECHO(h,bj,sj) from party Pj,
--   check that bj is a valid Merkle branch for root h and leaf sj, and otherwise discard
-- • upon receiving valid ECHO(h, ·, ·) messages from N − f distinct parties,
--   - interpolate {s′j } from any N − 2 f leaves received
--   – recompute Merkle root h′ and if h′ ̸= h then abort
--   – if READY(h) has not yet been sent, multicast READY(h)
-- • upon receiving f + 1 matching READY(h) messages, if READY has not yet been sent, multicast READY(h)
-- • upon receiving 2 f + 1 matching READY(h) messages, wait for N − 2 f ECHO messages, then decode v
--
-- The functions here are used to turn input into output. The IO is supposed to be handled separately.
--
-- The RBC module is not concerned about the authenticity of the communicators - verify validator's identity
-- before passing messages to RBC!

import           Data.ByteString     (ByteString)

import           Data.Serialize      (Serialize, decode)

import           Control.Monad.Catch (Exception (..), MonadCatch, MonadThrow,
                                      handle, throwM)
import           Control.Monad.State

import           ErasureCoding       (EncodingError (..), encodeByteString)

import           GHC.Generics        (Generic)

import           MerkleTree          (MerkleProof, mkMerkleProof)

type Validator = ByteString

type Shard = ByteString

data RBCState = RBCState {
     getValidators :: [Validator]
    ,getN          :: Int
    ,getF          :: Int
    ,getSelf       :: Validator
    ,getEcho       :: [(Validator, Maybe MerkleProof)]
    ,getReady      :: [Validator]
    ,getReadySent  :: Bool
}

data In =
      Input ByteString
    | Val MerkleProof
    | Echo MerkleProof
    | Ready MerkleProof
    deriving (Generic, Show)

instance Serialize In

data Out =
      Broadcast [(In, Validator)]
    | Output ByteString
    | None
    deriving Show

data RBCError =
      InputDecodingError String
    | ErasureCodingError String
    | UnknownValidator String
    | OwnMessageError String
    deriving Show

instance Exception RBCError

-- The first validator is always considered to be "self".
setup :: [Validator] -> RBCState
setup validators = RBCState {
     getValidators = validators
    ,getN = length validators
    ,getF = getByzantineToleranceNumber (length validators)
    ,getSelf = head validators
}

getByzantineToleranceNumber n | n <= 3 = 0
getByzantineToleranceNumber n = n `div` 3

getShardsNumber n f = if n < 4 then 1 else n - f -- we want RBC to work in 1, 2, and 3- validator networks as well but it's only BFT when there are at least 4 validators

receive :: (MonadThrow m, MonadCatch m) => ByteString -> State RBCState (m Out)
receive msg = case (decode msg) of
    Left msg      -> return $ throwM $ InputDecodingError msg
    Right decoded -> receive' decoded

receive' :: (MonadThrow m, MonadCatch m) => (In, Validator) -> State RBCState (m Out)
receive' (message, validator) = do
    state <- get
    if validator `elem` (getValidators state)
        then
            if validator == getSelf state
                then
                    return $ throwM $ OwnMessageError "Do not process my own messages"
                else
                    return $ receive'' message state
        else
            return $ throwM $ UnknownValidator $ "Do not know this validator: " ++ show validator

-- • upon input(v) (if Pi = PSender):
--   let {sj} j∈[N] be the blocks of an (N − 2f, N) erasure coding scheme applied to v
--   let h be a Merkle tree root computed over {sj}
--   send VAL(h,bj,sj) to each party Pj, where bj is the jth Merkle tree branch
receive'' (Input message) state = handle handleParseInputErrors (parseInput message state)

-- • upon receiving VAL(h,bi,si) from PSender, multicast ECHO(h,bi,si)
receive'' (Val proof) state = do
    return $ Broadcast (map (\v -> (Echo proof, v)) (drop 1 $ getValidators state))

parseInput message state = do
    shards <- encodeByteString (getN state, getShardsNumber (getN state) (getF state)) message -- {sj}
    return $ Broadcast (map (\(v, shard) -> (Val (mkMerkleProof shards shard), v)) (zip (drop 1 $ getValidators state) shards))

handleParseInputErrors (EncodingError msg) = throwM $ ErasureCodingError msg
