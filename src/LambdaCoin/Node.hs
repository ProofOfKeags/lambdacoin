module LambdaCoin.Node where

import           Basement.Types.Word256
import           Crypto.Hash
import           Crypto.Hash.Algorithms
import           Crypto.PubKey.ECC.ECDSA (Signature)
import qualified Data.HashSet as HS
import           Data.Time
import           Data.Word
import           Network.Socket (Socket, SockAddr)

data BlockChain = BlockChain 
    { uncles :: [Fork]
    , longest :: Fork
    }

data Fork = Fork
    { height :: Word32
    , blocks :: [Block]
    }

data Block = Block
    { blockhash :: BlockHash
    , timestamp :: UTCTime
    , txs :: [Transaction]
    , prev :: BlockHash
    }

type BlockHash = Digest SHA256
type Txid = Digest SHA256
type Index = Word32
type PubKeyHash = Digest RIPEMD160
type Value = Word64

data UTXO = UTXO
    { txid :: Txid
    , idx :: Index
    , dest :: PubKeyHash
    , value :: Value
    }

data Transaction = Tx
    { inputs :: [(UTXO, Signature)]
    , outputs :: [UTXO]
    }

data Peer = Peer
    { host :: SockAddr
    , connection :: Maybe Socket
    }

data Msg = NewTx Transaction
         | NewBlock Block

data Node = Node
    { chain :: BlockChain
    , peers :: [Peer]
    , utxos :: HS.HashSet UTXO
    , mempool :: [Transaction]
    , difficulty :: Word256
    }