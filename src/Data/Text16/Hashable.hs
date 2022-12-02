{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
module Data.Text16.Hashable () where

#include "MachDeps.h"

import Data.Hashable hiding (hashByteArrayWithSalt)

import Data.Bits
import GHC.Base (ByteArray#)

import qualified Data.Text16 as T16
import qualified Data.Text16.Array as TA
import qualified Data.Text16.Internal as T16
import qualified Data.Text16.Lazy as TL
import Foreign.C (CLong(..))

instance Hashable T16.Text16 where
    hashWithSalt salt (T16.Text16 arr off len) =
        hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1)
        salt

    hash = hashWithSalt legacyDefaultSalt

instance Hashable TL.Text16 where
    hashWithSalt = TL.foldlChunks hashWithSalt
    hash = hashWithSalt legacyDefaultSalt

-- | Compute a hash value for the content of this 'ByteArray#', using
-- an initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashByteArrayWithSalt
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Int         -- ^ salt
    -> Int         -- ^ hash value
hashByteArrayWithSalt ba !off !len !h =
    fromIntegral $ c_hashByteArray ba (fromIntegral off) (fromIntegral len)
    (fromIntegral h)

foreign import ccall unsafe "hashable_fnv_hash_offset_legacy" c_hashByteArray
    :: ByteArray# -> CLong -> CLong -> CLong -> CLong


-- | A default salt used in the implementation of 'legacyHash'.
legacyDefaultSalt :: Int
#if WORD_SIZE_IN_BITS == 64
legacyDefaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4
#else
legacyDefaultSalt = 0x087fc72c
#endif
