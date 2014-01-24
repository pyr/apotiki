module System.Apotiki.Ar (arFromData, arFromFile, ArEntry (..)) where
import Data.Attoparsec.Combinator (manyTill)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as P

armag = "!<arch>\n" -- magic header
arfmag = "`\n" -- magic pad

data ArEntry = ArEntry { -- A file entry in an ar file
  entryDate :: Int,
  entryGid  :: Int,
  entryUid  :: Int,
  entryMode :: Int,
  entrySize :: Int,
  entryData :: B.ByteString
  } deriving Show

type ArMapEntry = (String, ArEntry)
type ArMap = M.Map String ArEntry

arEntryParser :: P.Parser ArMapEntry
arEntryParser = do
  let not_space = (\c -> (c /= '/') && (c /= ' '))
  name <- (takeWhile not_space . unpack) `fmap` P.take 16
  date <- (read . unpack) `fmap` P.take 12
  gid <- (read . unpack) `fmap` P.take 6
  uid <- (read . unpack) `fmap` P.take 6
  mode <- (read . unpack) `fmap` P.take 8
  size <- (read . unpack) `fmap` P.take 10
  magic <- P.string $ pack arfmag
  payload <- P.take size
  padding <- if size `mod` 2 == 1 then P.string $ pack "\n" else P.take 0
  return (name, ArEntry date gid uid mode size payload)

arParser :: P.Parser ArMap
arParser = do
  magic <- P.string $ pack armag
  entries <- manyTill arEntryParser P.endOfInput
  return (M.fromList entries)

arFromData :: B.ByteString -> Either String ArMap
arFromData input = P.parseOnly arParser input

arFromFile :: String -> IO (Either String ArMap)
arFromFile path = do
  content <- B.readFile path
  return (arFromData content)
