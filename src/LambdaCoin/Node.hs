module LambdaCoin.Node where

import           Basement.Types.Word256
import qualified Data.HashSet as HS
import           Data.Word
import           Network.Socket (Socket, SockAddr)

import LambdaCoin.Block
import LambdaCoin.Transaction


data Fork = Fork
    { height :: Word32
    , blocks :: [Block]
    }

data BlockChain = BlockChain 
    { uncles :: [Fork]
    , longest :: Fork
    }

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
