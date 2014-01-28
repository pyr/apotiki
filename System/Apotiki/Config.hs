module System.Apotiki.Config where
import System.Apotiki.Utils
import Data.Attoparsec.Combinator (manyTill, many1)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (intersperse)
import Network.Wai.Handler.Warp(HostPreference(..), settingsHost, settingsPort)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as P

import Web.Scotty
import Data.Default (def)
import Network.Wai.Handler.Warp (settingsPort)

data ApotikiConfig = ApotikiConfig {
  keyPath :: String,
  architectures :: [String],
  component :: String,
  release :: String,
  label   :: String,
  origin  :: String,
  repoDir :: String,
  logPath :: String,
  waiOpts :: Options
  }

configKeyPath ApotikiConfig { keyPath = x } = x
configArchs ApotikiConfig { architectures = x } = x
configComponent ApotikiConfig { component = x } = x
configRelease ApotikiConfig { release = x } = x
configPoolDir ApotikiConfig { repoDir = x } = x ++ "/pool"
configDistDir ApotikiConfig { repoDir = x } = x ++ "/dists"
configRepoDir ApotikiConfig { repoDir = x } = x
configOrigin ApotikiConfig { origin = x } = x
configLabel ApotikiConfig { label = x } = x
configLogPath ApotikiConfig { logPath = x } = x
configWai ApotikiConfig { waiOpts = x } = x

type ConfigMap = M.Map String String

keyLineParser :: P.Parser String
keyLineParser = do
  P.skipWhile (P.inClass " \t")
  val <- unpack `fmap` P.takeWhile (P.notInClass "\n")
  P.string $ pack "\n"
  return $ strip val

keyParser :: P.Parser String
keyParser = do
  P.skipWhile (P.notInClass "\n")
  P.string $ pack "\n"
  key <- (concat . intersperse "\n") `fmap` many1 keyLineParser
  return $ key ++ "\n"

valParser :: P.Parser String
valParser = do
  P.skipWhile (P.inClass "\t ")
  v <- unpack `fmap` P.takeWhile (P.notInClass "\n")
  P.string $ pack "\n"
  return $ strip v

directiveParser :: P.Parser (String, String)
directiveParser = do
  k <- unpack `fmap` P.takeWhile (P.notInClass ":")
  P.string $ pack ":"
  P.skipWhile (P.inClass " \t")
  v <- if k == "pgp-key" then keyParser else valParser
  return (strip k, v)

configParser :: P.Parser ConfigMap
configParser = M.fromList `fmap` many1 directiveParser

parseData :: B.ByteString -> ApotikiConfig
parseData input = transformConfigMap config_map where
  parsed = P.parseOnly configParser input
  Right config_map = parsed

parseFile :: String -> IO (ApotikiConfig)
parseFile path = do
  content <- B.readFile path
  return (parseData content)

parseList :: String -> [String]
parseList input = if input == "" then [] else (h:(parseList $ strip t)) where
  (h,t) = break (== ' ') input

get_wai_port cfg = case (M.lookup "port" cfg) of
  Just x  -> (read x :: Int)
  Nothing -> 8000

get_wai_host cfg = case (M.lookup "host" cfg) of
  Nothing -> HostAny
  Just x -> Host x

get_wai_opts cfg =
  def { verbose = 0,
        settings = (settings def) { settingsPort = get_wai_port cfg,
                                    settingsHost = get_wai_host cfg
                                  }
      }

transformConfigMap :: ConfigMap -> ApotikiConfig
transformConfigMap cfg =
  ApotikiConfig {
    keyPath       = cfg M.! "pgp-key",
    release       = cfg M.! "release",
    component     = cfg M.! "component",
    label         = cfg M.! "label",
    origin        = cfg M.! "origin",
    repoDir       = cfg M.! "repo",
    logPath       = cfg M.! "logfile",
    architectures = parseList $ cfg M.! "architectures",
    waiOpts       = get_wai_opts cfg
    }
