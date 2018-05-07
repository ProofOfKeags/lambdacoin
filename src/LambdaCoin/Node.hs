module LambdaCoin.Node where

import           Basement.Types.Word256
import           Crypto.Hash
import           Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Hashable
import           Data.Time
import           Data.Word
import           Network.Socket (Socket, SockAddr)
import LambdaCoin.Keys

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
    deriving (Eq)

instance Hashable UTXO where
    hashWithSalt i u = hashWithSalt i ((BA.convert $ txid u :: ByteString), idx u)

data Transaction = Tx
    { inputs :: [(UTXO, (PublicKey, Signature))]
    , outputs :: [UTXO]
    }
    | Coinbase UTXO

data Peer = Peer
    { host :: SockAddr
    , connection :: Maybe Socket
    }

data Msg = NewTx Transaction
         | NewBlock Block
         | GetBlock BlockHash
         | GetCurrentBlock
         | GetPeers
         | SendPeers [Peer]

data Node = Node
    { chain :: BlockChain
    , peers :: [Peer]
    , utxos :: HS.HashSet UTXO
    , mempool :: [Transaction]
    , difficulty :: Word256
    }
