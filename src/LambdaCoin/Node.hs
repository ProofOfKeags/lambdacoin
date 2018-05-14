module LambdaCoin.Node where

import           Basement.Types.Word256
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Word
import           Network.Socket (Socket, SockAddr)

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
    , mempool :: HS.HashSet Transaction
    , difficulty :: Word256
    }
