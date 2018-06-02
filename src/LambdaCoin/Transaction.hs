{-# LANGUAGE RecordWildCards #-}
module LambdaCoin.Transaction where

import           Control.Applicative (liftA2)
import           Control.Monad (unless)
import           Crypto.Hash
import           Crypto.Random
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Hashable
import           Data.Serialize
import           Data.Traversable
import           Data.Word

import LambdaCoin.Hash
import LambdaCoin.Keys

-- DATATYPES
type Txid = Digest SHA256
type Index = Word32
type PubKeyHash = Digest RIPEMD160
type Value = Word64

type Input = UTXO
type SignedInput = (Input, (PublicKey, Signature))

data Output = Output
    { idx :: Index
    , dest :: PubKeyHash
    , value :: Value
    } deriving (Eq, Show)

data UnsignedTx = UnsignedTx
    { uInputs :: [Input]
    , uOutputs :: [Output]
    } deriving (Eq, Show)

data Transaction = Transaction
    { sInputs :: [SignedInput]
    , sOutputs :: [Output]
    } deriving (Eq, Show)

data UTXO = UTXO
    { txid :: Txid
    , output :: Output
    } deriving (Eq, Show)

-- INSTANCES
instance Hashable UTXO where
    hashWithSalt i utxo = hashWithSalt i ((BA.convert $ txid utxo :: ByteString), idx . output $ utxo)

instance Serialize UTXO where
    put u = do
        putByteString . BA.convert $ txid u
        put $ output u
    get = do
        txid' <- digestFromByteString <$> getByteString 32
        txid <- case txid' of
            Nothing -> fail "Could not read txid"
            Just x -> return x
        output <- get
        return UTXO{..}

instance Serialize Output where
    put o = do
        putWord32be $ idx o
        putByteString . BA.convert $ dest o
        putWord64be $ value o
    get = do
        idx <- getWord32be
        dest' <- digestFromByteString <$> getByteString 20
        dest <- case dest' of
            Nothing -> fail "Could not read destination"
            Just x -> return x
        value <- getWord64be
        return Output{..}


instance Serialize Transaction where
    put tx = do
        put $ sInputs tx
        put $ sOutputs tx
    get = liftA2 Transaction get get

instance Hashable Transaction where
    hashWithSalt i tx = hashWithSalt i (BA.convert . hash256 . encode $ tx :: ByteString)


instance Serialize UnsignedTx where
    put u = do
        put $ uInputs u
        put $ uOutputs u
    get = liftA2 UnsignedTx get get

toUnsigned :: Transaction -> UnsignedTx
toUnsigned Transaction{..} = UnsignedTx{..}
    where
        uInputs = fst <$> sInputs
        uOutputs = sOutputs

signTx :: [PrivateKey] -> UnsignedTx -> IO (Maybe Transaction)
signTx keys unsigned
    | (pubKeyHash . derivePubKey <$> keys) /= (dest . output <$> uInputs unsigned) = return Nothing
    | otherwise = do
        sInputs' <- sInputs''
        return $ case sInputs' of
            Nothing -> Nothing
            Just sInputs -> Just $ Transaction{..}
    where
        message = encode unsigned
        sigs = fmap sequenceA . for (zip keys $ uInputs unsigned) $ \(key, u) -> do
            n <- decode <$> getRandomBytes 32
            return $ case n of
                Left _ -> Nothing
                Right k -> signWith k key message
        pubkeys = derivePubKey <$> keys
        sOutputs = uOutputs unsigned
        sInputs'' = (fmap . fmap) (\s -> zip (uInputs unsigned) (zip pubkeys s)) sigs

getTxid :: Transaction -> Txid
getTxid = hash256 . encode

txUtxos :: Transaction -> [UTXO]
txUtxos tx = zipWith UTXO (repeat $ getTxid tx) (sOutputs tx)