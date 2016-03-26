{-# LANGUAGE OverloadedStrings #-}

-- | A format developed as part of the snappy java library.
--
-- From the docs:
--
-- > The output format is the stream header "snappy\0" followed by one or
-- > more compressed blocks of data, each of which is preceded by a seven
-- > byte header.
-- >
-- > The first byte of the header is a flag indicating if the block is
-- > compressed or not. A value of 0x00 means uncompressed, and 0x01 means
-- > compressed.
-- >
-- > The second and third bytes are the size of the block in the stream as
-- > a big endian number. This value is never zero as empty blocks are
-- > never written. The maximum allowed length is 32k (1 << 15).
-- >
-- > The remaining four byes are crc32c checksum of the user input data
-- > masked with the following function: {@code ((crc >>> 15) | (crc <<
-- > 17)) + 0xa282ead8 }
-- >
-- > An uncompressed block is simply copied from the input, thus
-- > guaranteeing that the output is never larger than the input (not
-- > including the header).
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  73 6e 61 70 70 79 00 00  00 07 3a bb 17 0a 66 6f  |snappy....:...fo|
-- > 00000010  6f 62 61 72 0a                                    |obar.|
--
-- Reference: https://github.com/dain/snappy
module Codec.Compression.Snappy.Framed.SnappyInJava
    ( parseHeader
    , parseBlock
    ) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)

-- | Attempt to parse the header. If the header exists, it will be
-- consumed. If not, the parser will fail.
parseHeader :: Parser ()
parseHeader =
    void $ AP.string "snappy\x00"

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: Parser ByteString
parseBlock =
    fail "Snappy.Framed.SnappyInJava: not implemented"
