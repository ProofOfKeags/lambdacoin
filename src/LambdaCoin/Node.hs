{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module LambdaCoin.Node where

import           Basement.Types.Word256 (Word256(..))
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe (catMaybes)
import           Data.Serialize
import           Data.Traversable
import           Data.Time
import           Data.Word
import           Network.Socket (Socket, SockAddr(..), tupleToHostAddress)
import           Network.Socket.ByteString (send, recv)

import LambdaCoin.Block
import LambdaCoin.TestData
import LambdaCoin.Hash
import LambdaCoin.Transaction

data BlockChain = BlockChain 
    { blocks :: HM.HashMap BlockHash (Word32, BlockHash)
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
    , difficulty :: Word256
    }

bootstrapPeer = Peer
    { host = SockAddrInet 8333 $ tupleToHostAddress (127, 0, 0, 1)
    , connection = Nothing
    }

defaultNode :: Node
defaultNode = Node
    { chain = BlockChain
        { blocks = HM.fromList [(hash256 . encode . header $ genesisBlock, (0, prev . header $ genesisBlock))]
        , tip = hash256 . encode . header $ genesisBlock
        }
    , peers = [bootstrapPeer]
    , utxos = HS.fromList (txUtxos . coinbaseTx $ genesisBlock)
    , mempool = HS.empty
    , difficulty = startingDifficulty
    }

startingDifficulty :: Word256
startingDifficulty = Word256
    0x00000FFFFFFFFFFF
    0xFFFFFFFFFFFFFFFF
    0xFFFFFFFFFFFFFFFF
    0xFFFFFFFFFFFFFFFF


mine :: Node -> BlockHeader -> BlockHeader
mine node bh = if (hash256asWord . hash256 . encode $ bh) < difficulty node
    then bh
    else mine node $ bh { nonce = nonce' }
    where
        nonce' = nonce bh + 1

processBlock :: Node -> Block -> Node
processBlock node block = node
    { mempool = newMemPool
    , utxos = newUtxos
    , chain = newChain
    }
    where
        newMemPool = HS.difference (mempool node) (HS.fromList $ standardTxs block)
        newUtxos = HS.union
            (HS.fromList $ (coinbaseTx block : standardTxs block) >>= txUtxos) 
            (HS.difference (utxos node) $ HS.fromList $ standardTxs block >>= fmap fst . sInputs)
        newChain = 
            let
                blockchain = blocks . chain $ node
                blockhash = hash256 . encode . header $ block
                lastHash = prev . header $ block
                lastBlock = HM.lookup lastHash blockchain
                base = HM.lookup (prev . header $ block) blockchain
            in case base of
                Nothing -> chain node -- invalid block return original chain
                Just (height, next) -> if
                    | (tip . chain $ node) == lastHash -> BlockChain
                        { blocks = HM.insert blockhash (height+1, lastHash) blockchain
                        , tip  = blockhash
                        }
                    | otherwise -> case HM.lookup (tip . chain $ node) (blocks . chain $ node) of
                        Nothing -> undefined -- unreachable
                        Just (heightTip, nextTip) -> if
                            | height+1 > heightTip -> BlockChain
                                { blocks = HM.insert blockhash (height+1, prev . header $ block) blockchain
                                , tip = blockhash
                                }
                            | otherwise -> BlockChain
                                { blocks = HM.insert blockhash (height+1, prev . header $ block) blockchain
                                , tip = tip . chain $ node
                                }

                    -- add block to hash map, if it builds on current tip update the tip, if it builds on something
                    -- else compare it to the tip, if the fork reorganizes, rewind UTXO's to common history, and play
                    -- UTXO's from new fork.

genesisBlock :: Block
genesisBlock = Block
    { header = genesisHeader
    , coinbaseTx = Transaction
        { sInputs = []
        , sOutputs = [Output
            { idx = 0
            , dest = pkHash
            , value = 50 * 100000000
            }]
        }
    , standardTxs = []
    }

genesisHeader :: BlockHeader
genesisHeader = BlockHeader
    { prev = hash256 $ ("Chancellor on brink of second bailout for banks" :: ByteString)
    , commitmentHash = hash256 $ ("Who is Satoshi Nakamoto?" :: ByteString)
    , timestamp = UTCTime (ModifiedJulianDay 54834) 0
    , nonce = 0
    }


