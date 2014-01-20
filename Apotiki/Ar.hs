module Apotiki.Ar (arFromData, arFromFile, ArEntry (..)) where
import Control.Exception.Base
import Data.Attoparsec.Combinator
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec.ByteString as P

armag = "!<arch>\n"
arfmag = "`\n"

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
  name_b <- P.take 16
  let name = takeWhile (/= '/') $ BC.unpack name_b
  date_b <- P.take 12
  let date = read (BC.unpack date_b) :: Int
  gid_b <- P.take 6
  let gid = read (BC.unpack gid_b) :: Int
  uid_b <- P.take 6
  let uid = read (BC.unpack uid_b) :: Int
  mode_b <- P.take 8
  let mode = read (BC.unpack mode_b) :: Int
  size_b <- P.take 10
  let size = read (BC.unpack size_b) :: Int
  magic <- P.string $ BC.pack arfmag
  payload <- P.take size
  padding <- if size `mod` 2 == 1 then
               P.string $ BC.pack "\n"
             else
               P.take 0
  return (name,
          ArEntry {
            entryDate = date,
            entryGid = gid,
            entryUid = uid,
            entryMode = mode,
            entrySize = size,
            entryData = payload
            })

arParser :: P.Parser ArMap
arParser = do
  magic <- P.string $ BC.pack armag
  entries <- manyTill arEntryParser P.endOfInput
  return (M.fromList entries)

arFromData :: B.ByteString -> Either String ArMap
arFromData input = P.parseOnly arParser input

arFromFile :: String -> IO (Either String ArMap)
arFromFile path = do
  content <- B.readFile path
  return (arFromData content)

test :: String -> IO ()
test path = do
  (Right ar_map) <- arFromFile path
  putStrLn $ show $ ar_map M.! "debian-binary"
