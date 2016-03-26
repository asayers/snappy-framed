# snappy-framed

This library provides decoding support for framed snappy streams. It is in a
state of partial usefulness (see [Current Status](#current-status) below). Go
to https://asayers.github.io/snappy-framed/ for documentation.

## Discussion

Snappy is a block compression format, meaning that the whole compressed stream
must be kept in memory until it is fully decoded [1]. The task of splitting
data into a stream of independently-decodable chunks is handled by the framing
format. In addition, this format often provides checksums of the uncompressed
data.

Unfortunately, for a long time snappy had no official framing format, and so a
number of improvised formats appeared. While there is now a standard format,
many of the historical formats are still in common use. The good news is that
these formats mercifully begin with distinct magic byte sequences, and so can
be easily distinguished.

The list of formats, and the names given to them, come from the snzip
application (https://github.com/kubo/snzip).

\[1\]: In Snappy, the offsets used by back-references may be as large as a
32-bit word. As a result, a byte in the uncompressed stream can't be discarded
until 4GB of uncompressed data following it has been decoded. This effectively
makes Snappy a block compression format.

## Current status

We can identify a number of different framing formats. The only formats we can
decode, however, are snappy-java and no-framing. (Kafka uses snappy-java; this
was my original use-case.) Adding support for framing2 should be
straightforward, since it's already implemented in the snappy-framed package.
