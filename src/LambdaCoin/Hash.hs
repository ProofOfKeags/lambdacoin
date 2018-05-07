module LambdaCoin.Hash where

import Crypto.Hash
import Data.ByteArray

hash160 :: ByteArrayAccess ba => ba -> Digest RIPEMD160
hash160 = hashWith RIPEMD160 . hashWith SHA256

hash256 :: ByteArrayAccess ba => ba -> Digest SHA256
hash256 = hashWith SHA256 . hashWith SHA256