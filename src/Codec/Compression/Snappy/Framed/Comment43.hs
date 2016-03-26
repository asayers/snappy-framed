{-# LANGUAGE OverloadedStrings #-}

-- | A format defined in a comment on an issue in the old snappy issue
-- tracker.
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  ff 06 00 73 6e 61 70 70  79 01 0b 00 0a 17 bb 3a  |...snappy......:|
-- > 00000010  66 6f 6f 62 61 72 0a fe  00 00                    |foobar....|
--
-- Reference: http://code.google.com/p/snappy/issues/detail?id=34#c43
--     (Broken link, not in the Wayback Machine. Lost to history?)
module Codec.Compression.Snappy.Framed.Comment43
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
    void $ AP.string "\xff\x06\x00snappy"

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: Parser ByteString
parseBlock =
    fail "Snappy.Framed.Comment43: not implemented"
