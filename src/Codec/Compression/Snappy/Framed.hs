{-# LANGUAGE LambdaCase #-}

-- | This module provides facilities for decoding framed Snappy streams.
--
-- Snappy is a block compression format, meaning that the whole
-- compressed stream must be kept in memory until it is fully decoded [1].
-- The task of splitting data into a stream of independently-decodable
-- chunks is handled by the framing format. In addition, this format often
-- provides checksums of the uncompressed data.
--
-- Unfortunately, for a long time snappy had no official framing format,
-- and so a number of improvised formats appeared. While there is now
-- a standard format, many of the historical formats are still in common
-- use. The good news is that these formats mercifully begin with distinct
-- magic byte sequences, and so can be easily distinguished.
--
-- The list of formats, and the names given to them, come from the snzip
-- application (https://github.com/kubo/snzip).
--
-- \[1\]: In Snappy, the offsets used by back-references may be as large as
-- a 32-bit word. As a result, a byte in the uncompressed stream can't be
-- discarded until 4GB of uncompressed data following it has been decoded.
-- This effectively makes Snappy a block compression format.
--
-- TODO (asayers): Tests
module Codec.Compression.Snappy.Framed
    ( decompress
    , decompress_
    , decompress__

    -- * Internals
    , FramingFormat(..)
    , parseHeader
    , parseBlock
    ) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import Data.Bifunctor
import Data.ByteString (ByteString)
import Pipes
import Pipes.Attoparsec

import qualified Codec.Compression.Snappy.Framed.Framing2 as Framing2
import qualified Codec.Compression.Snappy.Framed.Framing1 as Framing1
import qualified Codec.Compression.Snappy.Framed.SNZip as SNZip
import qualified Codec.Compression.Snappy.Framed.SnappyJava as SnappyJava
import qualified Codec.Compression.Snappy.Framed.SnappyInJava as SnappyInJava
import qualified Codec.Compression.Snappy.Framed.Comment43 as Comment43

-- | Decompress a framed Snappy stream, reporting errors.
decompress :: (Monad m) => ByteString -> Producer ByteString m (Either String ())
decompress = fmap (first (peMessage . fst)) . decompress__

-- | Decompress a framed Snappy stream, raising an exception on bad input.
-- TODO (asayers): better names
decompress_ :: (Monad m) => ByteString -> Producer ByteString m ()
decompress_ = fmap (either (error err) id) . decompress__
  where
    err = "Codec.Compression.Snappy.Extras.decompress': parse error"

-- | Decompress a framed Snappy stream, returning unconsumed input in the
-- case of an error.
-- TODO (asayers): we can do better in terms of streaming the input.
decompress__
    :: (Monad m)
    => ByteString
    -> Producer ByteString m (Either (ParsingError, Producer ByteString m ()) ())
decompress__ bs = do
    AP.Done remainder header <- pure $ AP.parse parseHeader bs
    parsed (parseBlock header) (yield remainder)

-------------------------------------------------------------------------------
-- Internals

-- | Snappy unfortunately has a variety of historical framing formats, and
-- while the comminity has now accepted "framing2" as the default, Kafka
-- still uses the "snappy-java" framing format.
data FramingFormat
    = Framing2      -- default        extension: sz
    | Framing1      -- obselete       extension: sz
    | SNZip         -- obsolete       extension: snz
    | SnappyJava    -- non-standard   extension: snappy
    | SnappyInJava  -- obsolete       extension: snappy
    | Comment43     -- obsolete       extension: snappy
    -- TODO (asayers): looks like hadoop-snappy might define its own
    -- format. Investigate.

-- | Attempt to parse the headers of each format in turn. This tells us
-- which format we're using.
parseHeader :: Parser FramingFormat
parseHeader = msum
    [ Framing2     <$ Framing2.parseHeader
    , Framing1     <$ Framing1.parseHeader
    , SNZip        <$ SNZip.parseHeader
    , SnappyJava   <$ SnappyJava.parseHeader
    , SnappyInJava <$ SnappyInJava.parseHeader
    , Comment43    <$ Comment43.parseHeader
    ]

-- | Parse a single block of the compressed bytestream, returning a segment
-- of the uncompressed stream.
parseBlock :: FramingFormat -> Parser ByteString
parseBlock = \case
    Framing2     -> Framing2.parseBlock
    Framing1     -> Framing1.parseBlock
    SNZip        -> SNZip.parseBlock
    SnappyJava   -> SnappyJava.parseBlock
    SnappyInJava -> SnappyInJava.parseBlock
    Comment43    -> Comment43.parseBlock
