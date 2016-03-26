{-# LANGUAGE OverloadedStrings #-}

-- | A format developed as part of the snzip application.
--
-- From the docs:
--
-- > The first three bytes are magic characters 'SNZ'.
-- >
-- > The fourth byte is the file format version. It is 0x01.
-- >
-- > The fifth byte is the order of the block size. The input data is
-- > divided into fixed-length blocks and each block is compressed by
-- > snappy. When it is 16 (default value), the block size is 16th power of
-- > 2; 64 kilobytes.
-- >
-- > The rest is pairs of a compressed data length and a compressed data
-- > block The compressed data length is encoded as
-- > snappy::Varint::Encode32() does. If the length is zero, it is the end
-- > of data.
-- >
-- > Though the rest after the end of data is ignored for now, they may be
-- > continuously read as a next compressed file as gzip does.
-- >
-- > Note that the uncompressed length of each compressed data block must
-- > be less than or equal to the block size specified by the fifth byte.
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  53 4e 5a 01 10 09 07 18  66 6f 6f 62 61 72 0a 00  |SNZ.....foobar..|
--
-- Reference: https://github.com/kubo/snzip
module Codec.Compression.Snappy.Framed.SNZip
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
    void $ AP.string "SNZ"

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: Parser ByteString
parseBlock =
    fail "Snappy.Framed.SNZip: not implemented"
