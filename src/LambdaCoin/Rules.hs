module LambdaCoin.Rules where

import LambdaCoin.Hash
import LambdaCoin.Node
import qualified Data.HashSet as HS
import Data.List
import Data.Monoid ((<>))
import Data.Serialize (encode, decode)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Control.Applicative
import Control.Monad.State

import LambdaCoin.Keys
import LambdaCoin.Transaction
import LambdaCoin.Block

type Rule a = Node -> a -> Bool

inputsSpendable :: Rule Transaction
inputsSpendable _ (Coinbase _) = True
inputsSpendable node tx = all inSet ins
    where
        inSet = flip HS.member $ utxos node
        ins = fst <$> inputs tx

noDuplicateInputs :: Rule Transaction
noDuplicateInputs _ (Coinbase _) = True
noDuplicateInputs node tx = 
    HS.size set == (length $ inputs tx)
    where
        set = HS.fromList . fmap fst $ inputs tx

inputsMature :: Rule Transaction
inputsMature _ (Coinbase _) = True
inputsMature node tx = all (mature . height . longest . chain $ node) $ fst <$> inputs tx
    where
        mature _ StandardOut{} = True
        mature h (CoinbaseOut _ _ h' _ _) = h >= h' + 50

atLeastOneInput :: Rule Transaction
atLeastOneInput _ (Coinbase _) = True
atLeastOneInput _ tx = not . Prelude.null $ inputs tx

atLeastOneOutput :: Rule Transaction
atLeastOneOutput _ (Coinbase _) = True
atLeastOneOutput _ tx = not . null $ outputs tx

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

transactionRules :: [Rule Transaction]
transactionRules =
    [ inputsSpendable
    , noDuplicateInputs
    , inputsMature
    , atLeastOneInput
    , atLeastOneOutput
    , inputsExceedOutputs
    , pubkeysMatchHashes
    , signaturesAreValid
    ]

isValidTransaction :: Rule Transaction
isValidTransaction node =
    foldl' (.&&.) (const True) $ ($ node) <$> transactionRules
    where 
        (.&&.) = liftA2 (&&)

blockUnderSizeLimit :: Rule Block
blockUnderSizeLimit _ block =
    BS.length (encode block) < limit
    where
        limit = 0x100000 -- #No2X

processTransaction :: Transaction -> State Node Bool
processTransaction tx = do
    node <- get
    let utxoSet = utxos node
        spent = fst <$> inputs tx
        newUtxoSet = HS.difference utxoSet (HS.fromList spent)
    if not $ isValidTransaction node tx
        then return False
        else (put $ node { utxos = newUtxoSet }) >> return True
