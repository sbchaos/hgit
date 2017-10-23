module Data.Compress.Compress where


import qualified Codec.Compression.Zlib as Z (compress, decompress)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString        as B

compress :: ByteString -> ByteString
compress   = toStrict . Z.compress   . fromStrict

decompress :: ByteString -> ByteString
decompress = toStrict . Z.decompress . fromStrict
