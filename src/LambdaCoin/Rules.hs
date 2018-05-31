module LambdaCoin.Rules where

import LambdaCoin.Hash
import LambdaCoin.Node
import qualified Data.HashMap.Strict as HM
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
inputsSpendable node tx = all inSet ins
    where
        inSet = flip HS.member $ utxos node
        ins = fst <$> sInputs tx

noDuplicateInputs :: Rule Transaction
noDuplicateInputs node tx = 
    HS.size set == length inputs
    where
        inputs = sInputs tx
        set = HS.fromList $ fst <$> inputs

noDuplicateOutputIndices :: Rule Transaction
noDuplicateOutputIndices _ tx =
    HS.size set == length outputs
    where
        outputs = sOutputs tx
        set = HS.fromList $ idx <$> outputs

atLeastOneInput :: Rule Transaction
atLeastOneInput _ tx = not . null $ sInputs tx

noInputs :: Rule Transaction
noInputs _ = null . sInputs

atLeastOneOutput :: Rule Transaction
atLeastOneOutput _ tx = not . null $ sOutputs tx

inputsExceedOutputs :: Rule Transaction
inputsExceedOutputs _ tx = (sum $ value . output . fst <$> ins) >= (sum $ value <$> outs)
    where
        ins = sInputs tx
        outs = sOutputs tx

pubkeysMatchHashes :: Rule Transaction
pubkeysMatchHashes _ tx = all pubkeyMatchesHash $ sInputs tx
    where
        pubkeyMatchesHash (utxo, (pk, _)) = pubKeyHash pk == dest (output utxo)

signaturesAreValid :: Rule Transaction
signaturesAreValid _ tx = all validSig $ sInputs tx
    where
        validSig (utxo, (pk, sig)) = 
            let output = BA.convert . hash256 . encode . toUnsigned $ tx
            in verifySig pk sig output

standardRules :: [Rule Transaction]
standardRules =
    [ inputsSpendable
    , noDuplicateInputs
    , atLeastOneInput
    , atLeastOneOutput
    , inputsExceedOutputs
    , pubkeysMatchHashes
    , signaturesAreValid
    ]

coinbaseRules :: [Rule Transaction]
coinbaseRules =
    [ atLeastOneOutput
    , noInputs
    ]

isValidTransaction :: [Rule Transaction] -> Rule Transaction
isValidTransaction rules node =
    foldl' (<&&>) (const True) $ ($ node) <$> rules
    where 
        (<&&>) = liftA2 (&&)

blockUnderSizeLimit :: Rule Block
blockUnderSizeLimit _ block =
    BS.length (encode block) < limit
    where
        limit = 0x100000 -- #No2X

processTransaction :: Node -> Transaction -> Node
processTransaction node tx =
    let
        utxoSet = utxos node
        spent = HS.fromList $ fst <$> sInputs tx
        created = HS.fromList $ zipWith UTXO (repeat $ getTxid tx) (sOutputs tx)
        newUtxoSet = HS.union created $ HS.difference utxoSet spent
    in node { utxos = newUtxoSet }

allTransactionsValid :: Rule Block
allTransactionsValid node block =
    isValidTransaction coinbaseRules node (coinbaseTx block)
    && transactionSeriesValid (processTransaction node (coinbaseTx block)) (standardTxs block)
    where
        transactionSeriesValid :: Node -> [Transaction] -> Bool
        transactionSeriesValid node [] = True
        transactionSeriesValid node (tx:txs) =
            let isValid = isValidTransaction standardRules node tx
            in if isValid
                then transactionSeriesValid (processTransaction node tx) txs
                else False

isValidBlock :: Rule Block
isValidBlock node =
    foldl' (<&&>) (const True) $ ($ node) <$> rules
    where
        (<&&>) = liftA2 (&&)
        rules = [
            blockUnderSizeLimit,
            allTransactionsValid -- TODO finish adding rules
            ]