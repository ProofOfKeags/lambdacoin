module LambdaCoin.Block where

import Crypto.Hash
import Data.Time
import Data.Word

import LambdaCoin.Transaction

type BlockHash = Digest SHA256
type MerkleRoot = Digest SHA256

data BlockHeader = BlockHeader
    { prev :: BlockHash
    , merkleRoot :: MerkleRoot
    , timestamp :: UTCTime
    , nonce :: Word32
    }


data Block = Block
    { blockhash :: BlockHash
    , header :: BlockHeader
    , txs :: [Transaction]
    }
