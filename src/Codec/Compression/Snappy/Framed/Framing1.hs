{-# LANGUAGE OverloadedStrings #-}

-- | An old version of the official format.
--
-- The name "framing1" refers to the format specified in the following
-- revision of the official snappy repo:
--
-- - framing format revision 2011-12-15, snappy 1.0.5, svn r55, git 0755c81
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  ff 06 00 73 4e 61 50 70  59 01 0b 00 0a 17 bb 3a  |...sNaPpY......:|
-- > 00000010  66 6f 6f 62 61 72 0a                              |foobar.|
--
-- Reference: https://github.com/google/snappy/blob/0755c81/framing_format.txt
module Codec.Compression.Snappy.Framed.Framing1
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
    void $ AP.string "\xff\x06\x00sNaPpY"

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: Parser ByteString
parseBlock =
    fail "Snappy.parseBlock: Framing1 not implemented"
