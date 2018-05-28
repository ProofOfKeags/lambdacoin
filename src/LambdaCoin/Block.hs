{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LambdaCoin.Block where

import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Serialize
import Data.Time
import Data.Word

import LambdaCoin.Transaction

newtype BlockHash = BlockHash (Digest SHA256)
    deriving (Eq, BA.ByteArrayAccess)
instance Hashable BlockHash where
    hashWithSalt i bh = hashWithSalt i (BA.convert bh :: ByteString) 

data BlockHeader = BlockHeader
    { prev :: BlockHash
    , commitmentHash :: Digest SHA256
    , timestamp :: UTCTime
    , nonce :: Word32
    }

data Block = Block
    { blockhash :: BlockHash
    , header :: BlockHeader
    , coinbaseTx :: Transaction
    , standardTxs :: [Transaction]
    }

instance Serialize Block where
    put = _
    get = _

instance Serialize BlockHeader where
    put = _
    get = _