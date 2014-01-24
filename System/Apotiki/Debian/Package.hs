module System.Apotiki.Debian.Package where
import System.Apotiki.Ar
import System.Apotiki.Tar
import System.Apotiki.FileInfo
import System.Apotiki.Utils
import System.Apotiki.Config
import System.Apotiki.Debian.Control
import Data.List
import System.Directory
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M

writeToPool :: String -> (DebInfo, B.ByteString) -> IO ()
writeToPool repodir (info, payload) = do
  let path = info M.! "Filename"
  let dir_path = reverse $ snd $ break (== '/') $ reverse path
  putStrLn $ "found filename: " ++ path
  createDirectoryIfMissing True (repodir ++ "/" ++ dir_path)
  B.writeFile (repodir ++ "/" ++ path) payload
  B.writeFile (repodir ++ "/" ++ dir_path ++ "control") $ pack (show info)

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
    path = "pool/" ++ arch ++ "/" ++ pkg ++ "/" ++ pkg ++ ".deb"
