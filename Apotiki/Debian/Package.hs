module Apotiki.Debian.Package where
import Apotiki.Ar
import Apotiki.Tar
import Apotiki.FileInfo
import Apotiki.Utils
import Apotiki.Config
import Apotiki.Debian.Control
import Data.List
import System.Directory
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M

writeToPool :: String -> (DebInfo, B.ByteString) -> IO ()
writeToPool pooldir (info, payload) = do
  let path = info M.! "Filename"
  let dir_path = reverse $ snd $ break (== '/') $ reverse path
  putStrLn $ "found filename: " ++ path
  putStrLn $ "will create: " ++ dir_path
  createDirectoryIfMissing True (pooldir ++ "/" ++ dir_path)
  putStrLn $ "creating deb in: " ++ (pooldir ++ "/" ++ dir_path)
  B.writeFile (pooldir ++ "/" ++ path) payload
  putStrLn $ "creating deb control in: " ++ (pooldir ++ "/" ++ dir_path ++ "control")
  writeFile (pooldir ++ "/" ++ dir_path ++ "control") (show info)

toDebInfo :: String -> DebInfo
toDebInfo input = output where Right output = ctlFromData $ pack $ input

debInfo :: ApotikiConfig -> B.ByteString -> DebInfo
debInfo config payload =
  M.union (fileinfo payload) (M.insert "Filename" path debinfo) where
    Right archive = (arFromData payload)
    ArEntry {entryData = entry} = archive M.! "control.tar.gz"
    debinfo = toDebInfo $ getStrictControl entry
    arch = case M.lookup "Architecture" debinfo of
      Nothing -> "NOARCH"
      Just x -> x
    pkg = case M.lookup "Package" debinfo of
      Nothing -> "NOPKG"
      Just x -> x
    pooldir = configPoolDir config
    path = arch ++ "/" ++ pkg ++ "/" ++ pkg ++ ".deb"
