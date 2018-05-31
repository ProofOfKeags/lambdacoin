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

newtype TxArchive = TxArchive { archive :: HM.HashMap BlockHash [Transaction] }

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
        newMemPool = _ -- prune mempool of transactions in the block
        newUtxos = _ -- remove consumed UTXO's, add produced UTXO's
        newChain = _ -- add block to hash map, if it builds on current tip update the tip, if it builds on something
                     -- else compare it to the tip, if the fork reorganizes, rewind UTXO's to common history, and play
                     -- UTXO's from new fork.