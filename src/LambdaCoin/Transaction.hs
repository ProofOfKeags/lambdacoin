module LambdaCoin.Transaction where

import           Crypto.Hash
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.Hashable
import           Data.Serialize
import           Data.Word

import LambdaCoin.Keys

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
    hashWithSalt i utxo = hashWithSalt i ((BA.convert $ txid utxo :: ByteString), idx utxo)

data Transaction = Transaction
    { inputs :: [(UTXO, (PublicKey, Signature))]
    , outputs :: [UTXO]
    }

instance Serialize Transaction where
    put = _
    get = _