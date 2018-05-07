{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans.Word256 where

import Data.Serialize
import Basement.Types.Word256

instance Serialize Word256 where
    put (Word256 a b c d) = do
        putWord64be a
        putWord64be b
        putWord64be c
        putWord64be d
    get = do
        a <- getWord64be
        b <- getWord64be
        c <- getWord64be
        d <- getWord64be
        return $ Word256 a b c d