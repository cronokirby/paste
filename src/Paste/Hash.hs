{-# LANGUAGE OverloadedStrings #-}
module Paste.Hash
    ( hashBase64
    , toPath
    , unpackHash
    , Hash(..)
    )
where

import Crypto.Hash (hash, Digest, SHA256(..))
import Data.Byteable (toBytes)
import Data.ByteArray.Encoding (convertToBase, Base (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text (cons, Text)
import Data.Text.Encoding (decodeUtf8)


-- | Represents a hash of a file
newtype Hash = Hash { unHash :: ByteString } deriving (Eq, Show)

-- | Converts a hash to a string, safely.
unpackHash :: Hash -> String
unpackHash (Hash bs) = unpack bs

toPath :: Hash -> Text
toPath (Hash bs) = cons '/' (decodeUtf8 bs)

-- | Creates a hash of a sequence of bytes, in a url safe way
-- Uses SHA256 as a hashing algorithm, and an unpadded url-safe base64 encoding
-- following that.
-- This can be safely converted to a string
hashBase64 :: ByteString -> Hash
hashBase64 bs = Hash . convertToBase Base64URLUnpadded . toBytes $ (hash bs :: Digest SHA256)