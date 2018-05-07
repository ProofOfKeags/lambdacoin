module LambdaCoin.Rules where

import LambdaCoin.Hash
import LambdaCoin.Node
import Data.HashSet
import Data.Monoid ((<>))
import Data.Serialize
import qualified Data.ByteArray as BA

import LambdaCoin.Keys


type Rule a = Node -> a -> Bool

inputsSpendable :: Rule Transaction
inputsSpendable node tx = all inSet ins
    where
        inSet = flip member $ utxos node
        ins = fst <$> inputs tx

atLeastOneInput :: Rule Transaction
atLeastOneInput _ tx = (length $ inputs tx) > 0

atLeastOneOutput :: Rule Transaction
atLeastOneOutput _ tx = (length $ outputs tx) > 0

inputsExceedOutputs :: Rule Transaction
inputsExceedOutputs _ tx = (value . fst <$> ins) >= (value <$> outs)
    where
        ins = inputs tx
        outs = outputs tx

pubkeysMatchHashes :: Rule Transaction
pubkeysMatchHashes _ tx = all pubkeyMatchesHash $ inputs tx
    where
        pubkeyMatchesHash (utxo, (pk, _)) = hash160 (encode pk) == dest utxo

signaturesAreValid :: Rule Transaction
signaturesAreValid _ tx = all validSig $ inputs tx
    where
        validSig (utxo, (pk, sig)) = verifySig pk sig (BA.convert (txid utxo) <> encode (idx utxo))