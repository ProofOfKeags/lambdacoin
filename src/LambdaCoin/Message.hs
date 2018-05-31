module LambdaCoin.Message where

import           Control.Applicative
import qualified Data.ByteArray as BA
import qualified Data.HashSet as HS
import           Data.Serialize
import           Data.Word

import LambdaCoin.Block
import LambdaCoin.Node
import LambdaCoin.Rules
import LambdaCoin.Transaction

data Msg = NewTx Transaction
         | NewBlock Block

instance Serialize Msg where
    put (NewTx tx) = putWord8 0x00 >> put tx
    put (NewBlock b) = putWord8 0x01 >> put b
    get = do
        msgType <- getWord8
        case msgType of
            0x01 -> NewTx <$> get
            0x02 -> NewBlock <$> get
            _ -> empty

processMsg :: Node -> Msg -> Maybe Node
processMsg node msg = case msg of
    NewTx tx -> let currMemPool = mempool node
        in if isValidTransaction standardRules node tx
            then Just $ node { mempool = HS.insert tx currMemPool }
            else Nothing
    NewBlock b -> let currBlockChain = chain node
        in if isValidBlock node b
            then Just $ processBlock node b
            else Nothing

