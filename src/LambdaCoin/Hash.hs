module LambdaCoin.Hash where

import Basement.Types.Word256
import Crypto.Hash
import Data.ByteArray
import Data.Serialize
import Orphans.Word256

hash160 :: ByteArrayAccess ba => ba -> Digest RIPEMD160
hash160 = hashWith RIPEMD160 . hashWith SHA256

hash256 :: ByteArrayAccess ba => ba -> Digest SHA256
hash256 = hashWith SHA256 . hashWith SHA256

hash256asWord :: Digest SHA256 -> Word256
hash256asWord h = case decode $ convert h of
    Left _ -> undefined
    Right x -> x