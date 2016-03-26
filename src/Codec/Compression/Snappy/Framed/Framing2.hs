{-# LANGUAGE OverloadedStrings #-}

-- | The current version of the official format.
--
-- The name "framing2" refers to the format which was specified and then
-- (backwards-compatibly) modified in the following revisions of the
-- official snappy repo:
--
-- - framing format revision 2013-01-05, snappy 1.1.0, svn r71, git 27a0cc3
-- - framing format revision 2013-10-25, snappy 1.1.2, svn r82, git f82bff6
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  ff 06 00 00 73 4e 61 50  70 59 01 0b 00 00 0a 17  |....sNaPpY......|
-- > 00000010  bb 3a 66 6f 6f 62 61 72  0a                       |.:foobar.|
--
-- Reference: https://github.com/google/snappy/blob/f82bff6/framing_format.txt
module Codec.Compression.Snappy.Framed.Framing2
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
    void $ AP.string "\xff\x06\x00\x00sNaPpY"

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
--
-- TODO (asayers): defer to snappy-framing package, which implements this
-- format.
parseBlock :: Parser ByteString
parseBlock =
    fail "Snappy.Framed.Framing2: not implemented"
