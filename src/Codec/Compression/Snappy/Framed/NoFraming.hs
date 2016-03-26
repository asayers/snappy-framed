{-# LANGUAGE OverloadedStrings #-}

-- | An unframed snappy stream.
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  07 18 66 6f 6f 62 61 72  0a                       |..foobar.|
--
-- Reference: https://github.com/google/snappy/blob/master/format_description.txt
module Codec.Compression.Snappy.Framed.NoFraming
    ( parseBlock
    ) where

import qualified Codec.Compression.Snappy as Snappy
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)

-- The snappy format itself doesn't have a concept of a header.
-- parseHeader :: Parser ()

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: Parser ByteString
parseBlock =
    Snappy.decompress <$> AP.takeByteString
