{-# LANGUAGE OverloadedStrings #-}

-- | A format developed as part of the snappy-java library.
--
-- From the docs:
--
-- > SnappyOutputStream and SnappyInputStream use `[magic header:16
-- > bytes]([block size:int32][compressed data:byte array])*` format
--
-- The following example encoding was produced on an x86 machine, yet the
-- "block size" int32 appears to be big-endian. Therefore, I'm assuming
-- that this is an unwritten part of the spec.
--
-- Example encoding of the string "foobar\\n":
--
-- > 00000000  82 53 4e 41 50 50 59 00  00 00 00 01 00 00 00 01  |.SNAPPY.........|
-- > 00000010  00 00 00 09 07 18 66 6f  6f 62 61 72 0a           |......foobar.|
--
-- Reference: http://code.google.com/p/snappy-java/
module Codec.Compression.Snappy.Framed.SnappyJava
    ( parseHeader
    , parseBlock
    ) where

import qualified Codec.Compression.Snappy as Snappy
import Control.Monad
import qualified Data.Attoparsec.Binary as AP
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)
import Data.Int

-- | Attempt to parse the header. If the header exists, it will be
-- consumed. If not, the parser will fail.
parseHeader :: Parser ()
parseHeader = do
    void $ AP.string "\x82SNAPPY\x00"    -- 8-byte magic header
    void $ AP.string "\x00\x00\x00\x01"  -- 4-byte version number
    void $ AP.string "\x00\x00\x00\x01"  -- 4-byte min-compatible version

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: Parser ByteString
parseBlock = do
    blockLen <- anyInt32be
    blockData <- AP.take (fromIntegral blockLen)
    return $ Snappy.decompress blockData

-------------------------------------------------------------------------------
-- Helpers

{-# INLINE anyInt32be #-}
anyInt32be :: Parser Int32
anyInt32be = fromIntegral <$> AP.anyWord32be
