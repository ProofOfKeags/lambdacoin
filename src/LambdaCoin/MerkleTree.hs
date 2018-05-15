module LambdaCoin.MerkleTree where

import Crypto.Hash
import Data.Bits
import Data.Monoid ((<>))
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import Control.Arrow hiding (left, right)
import Data.Serialize (encode)

import LambdaCoin.Transaction

data MerkleNode =
      MerkleEmpty
    | MerkleBranch
        { root :: Digest SHA256
        , left :: MerkleNode
        , right :: MerkleNode
        }
    | MerkleLeaf
        { root :: Digest SHA256
        , tx :: Transaction
        }

mkHash :: Digest SHA256 -> Digest SHA256 -> Digest SHA256
mkHash a b = hashWith SHA256 $ (BA.convert a <> BA.convert b :: ByteString)

mkBranch :: MerkleNode -> MerkleNode -> MerkleNode
mkBranch a@MerkleLeaf{} b@MerkleEmpty = MerkleBranch { root = r, left = a, right = b } 
    where
        r = mkHash (root a) (root a)
mkBranch a@MerkleLeaf{} b@MerkleLeaf{} = MerkleBranch { root = r, left = a, right = b }
    where
        r = mkHash (root a) (root b)
mkBranch a@MerkleBranch{} b@MerkleEmpty = MerkleBranch { root = r, left = a, right = b }
    where
        r = mkHash (root a) (root a)
mkBranch a@MerkleBranch{} b@MerkleBranch{} = MerkleBranch { root = r, left = a, right = b }
    where
        r = mkHash (root a) (root b)
mkBranch _ _ = error "unreachable!"

mkLeaf :: Transaction -> MerkleNode
mkLeaf = uncurry MerkleLeaf . (hashWith SHA256 . encode &&& id)

mkTree :: [Transaction] -> MerkleNode
mkTree list
    | pow2 (length list) == length list =
        let half = length list `div` 2
            left = take half list
            right = drop half list
        in mkBranch (mkTree left) (mkTree right)
    | otherwise =
        let left = take (pow2 $ length list) list
            right = drop (pow2 $ length list) list
        in mkBranch (mkTree left) (mkTree right)

pow2 :: (Num a, Bits a) => a -> a
pow2 a
    | a .&. (a - 1) == 0 = a
    | otherwise = 2 * (pow2 $ a `shiftR` 2)