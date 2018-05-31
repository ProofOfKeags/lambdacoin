module LambdaCoin.Node where

import           Basement.Types.Word256
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe (catMaybes)
import           Data.Serialize
import           Data.Traversable
import           Data.Word
import           Network.Socket (Socket, SockAddr(..))
import           Network.Socket.ByteString (send, recv)

import LambdaCoin.Block
import LambdaCoin.Transaction

data BlockChain = BlockChain 
    { blocks :: HM.HashMap BlockHash (Word32, Word256, BlockHash)
    , tip :: BlockHash
    }

data Peer = Peer
    { host :: SockAddr
    , connection :: Maybe Socket
    }

data Node = Node
    { chain :: BlockChain
    , peers :: [Peer]
    , utxos :: HS.HashSet UTXO
    , mempool :: HS.HashSet Transaction
    }

processBlock :: Node -> Block -> Node
processBlock node block = node
    { mempool = newMemPool
    , utxos = newUtxos
    , chain = newChain
    }
    where
        newMemPool = _
        newUtxos = _
        newChain = _