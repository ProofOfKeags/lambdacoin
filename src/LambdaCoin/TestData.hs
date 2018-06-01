{-# LANGUAGE OverloadedStrings #-}
module LambdaCoin.TestData where

import Basement.Types.Word256 (Word256(..))
import Data.ByteString (ByteString)

import LambdaCoin.Hash
import LambdaCoin.Keys
import LambdaCoin.Transaction

priv :: PrivateKey
priv = PrivateKey $ Word256 0x41 0x41 0x41 0x41

pub :: PublicKey
pub = derivePubKey priv

pkHash :: PubKeyHash
pkHash = pubKeyHash pub

utxoFrom :: UTXO
utxoFrom = UTXO
    { txid = hash256 ("test" :: ByteString)
    , output = Output
        { idx = 0
        , dest = pkHash
        , value = 100000000
        }
    }

outputTo :: Output
outputTo = Output
    { idx = 0
    , dest = pkHash
    , value = 100000000
    }

unsignedTx :: UnsignedTx
unsignedTx = UnsignedTx
    { uInputs = [utxoFrom]
    , uOutputs = [outputTo]
    }
