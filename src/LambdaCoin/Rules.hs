module LambdaCoin.Rules where

import LambdaCoin.Hash
import LambdaCoin.Node
import Data.HashSet
import Data.Monoid ((<>))
import Data.Serialize
import qualified Data.ByteArray as BA

import LambdaCoin.Keys
import LambdaCoin.Transaction

type Rule a = Node -> a -> Bool

inputsSpendable :: Rule Transaction
inputsSpendable _ (Coinbase _) = True
inputsSpendable node tx = all inSet ins
    where
        inSet = flip member $ utxos node
        ins = fst <$> inputs tx

inputsMature :: Rule Transaction
inputsMature _ (Coinbase _) = True
inputsMature node tx = all (mature $ height . longest . chain $ node) $ fst <$> inputs tx
    where
        mature _ (StandardOut _ _ _ _) = True
        mature h (CoinbaseOut _ _ h' _ _) = h >= h' + 50

atLeastOneInput :: Rule Transaction
atLeastOneInput _ (Coinbase _) = True
atLeastOneInput _ tx = (length $ inputs tx) > 0

atLeastOneOutput :: Rule Transaction
atLeastOneOutput _ (Coinbase _) = True
atLeastOneOutput _ tx = (length $ outputs tx) > 0

inputsExceedOutputs :: Rule Transaction
inputsExceedOutputs _ (Coinbase _) = True
inputsExceedOutputs _ tx = (value . fst <$> ins) >= (value <$> outs)
    where
        ins = inputs tx
        outs = outputs tx

pubkeysMatchHashes :: Rule Transaction
pubkeysMatchHashes _ (Coinbase _) = True
pubkeysMatchHashes _ tx = all pubkeyMatchesHash $ inputs tx
    where
        pubkeyMatchesHash (utxo, (pk, _)) = hash160 (encode pk) == dest utxo

signaturesAreValid :: Rule Transaction
signaturesAreValid _ (Coinbase _) = True
signaturesAreValid _ tx = all validSig $ inputs tx
    where
        validSig (utxo, (pk, sig)) = 
            let output = BA.convert (txid utxo) <> encode (idx utxo)
            in verifySig pk sig output