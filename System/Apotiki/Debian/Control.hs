module System.Apotiki.Debian.Control (DebInfo, ctlFromData) where
import System.Apotiki.Utils
import Data.Attoparsec.Combinator (manyTill, many1)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as P

type DebInfo = M.Map String String

ctlValParser :: P.Parser String
ctlValParser = do
  P.string $ pack " "
  val <- unpack `fmap` P.takeWhile (P.notInClass "\n")
  P.string $ pack "\n"
  return val

ctlFlatDescParser = (concat . intersperse "\n ") `fmap` many1 ctlValParser

ctlEntryParser :: P.Parser (String, String)
ctlEntryParser = do
  k <- unpack `fmap` P.takeWhile (P.notInClass ":")
  P.string $ pack ":"
  v <- if (k == "Description") then ctlFlatDescParser else ctlValParser
  return (k, strip v)

ctlParser :: P.Parser DebInfo
ctlParser = M.fromList `fmap` many1 ctlEntryParser

ctlFromData :: B.ByteString -> Either String DebInfo
ctlFromData input = P.parseOnly ctlParser input

ctlFromFile :: String -> IO (Either String DebInfo)
ctlFromFile path = do
  content <- B.readFile path
  return (ctlFromData content)
