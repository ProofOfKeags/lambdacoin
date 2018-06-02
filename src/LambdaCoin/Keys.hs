{-# LANGUAGE NoImplicitPrelude #-}
module LambdaCoin.Keys
    ( signWith
    , verifySig
    , derivePubKey
    , pubKeyHash
    , PublicKey(..)
    , PrivateKey(..)
    , Signature(..)
    ) where

import Foundation
import           Basement.Types.Word256 (Word256(..))
import qualified Crypto.PubKey.ECC.ECDSA as CN
import qualified Crypto.PubKey.ECC.Generate as CN
import qualified Crypto.PubKey.ECC.Types as CN
import           Crypto.Hash
import           Crypto.Hash (SHA256(..))
import           Data.ByteString (ByteString)
import           Data.Serialize (Serialize(..), encode)
import           Orphans.Word256 ()
import LambdaCoin.Hash

data PublicKey = PublicKey Word256 Word256
    deriving (Eq, Show)
data PrivateKey = PrivateKey Word256
data Signature = Signature Word256 Word256
    deriving (Eq, Show)

curve :: CN.Curve
curve = CN.getCurveByName CN.SEC_p256k1

toPubCN :: PublicKey -> CN.PublicKey
toPubCN (PublicKey x y) = CN.PublicKey curve $ CN.Point (toInteger x) (toInteger y)

toPrivCN :: PrivateKey -> CN.PrivateKey
toPrivCN (PrivateKey n) = CN.PrivateKey curve (toInteger n)

fromSigCN :: CN.Signature -> Signature
fromSigCN sig = Signature (fromInteger . CN.sign_r $ sig) (fromInteger . CN.sign_s $ sig)

toSigCN :: Signature -> CN.Signature
toSigCN (Signature r s) = CN.Signature (toInteger r) (toInteger s)

signWith :: Word256 -> PrivateKey -> ByteString -> Maybe Signature
signWith i s m = fromSigCN <$> CN.signWith (toInteger i) (toPrivCN s) SHA256 m

verifySig :: PublicKey -> Signature -> ByteString -> Bool
verifySig pk sig msg = CN.verify SHA256 (toPubCN pk) (toSigCN sig) msg

derivePubKey :: PrivateKey -> PublicKey
derivePubKey (PrivateKey n) = PublicKey (fromInteger x) (fromInteger y)
    where
        (CN.Point x y) = CN.generateQ curve (toInteger n)

instance Serialize PublicKey where
    put (PublicKey x y) = put x >> put y
    get = liftA2 PublicKey get get

instance Serialize Signature where
    put (Signature r s) = put r >> put s
    get = liftA2 Signature get get

pubKeyHash :: PublicKey -> Digest RIPEMD160
pubKeyHash = hash160 . encode
