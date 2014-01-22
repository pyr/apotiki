module Apotiki.Debian.Release where
import Apotiki.Debian.Package
import Apotiki.Debian.Control
import Apotiki.FileInfo
import Apotiki.Config
import Apotiki.Signature
import System.Directory
import Data.List
import Data.Function
import Data.ByteString.Char8 (pack,unpack)
import qualified Codec.Compression.GZip as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

type ArchRelease = M.Map String DebInfo
type Release = M.Map String ArchRelease

flatinfo fileinfo = [fileinfo M.! "Size", fileinfo M.! "MD5sum",
                     fileinfo M.! "SHA1", fileinfo M.! "SHA256"]

unrollEntry (k,v) = k ++ ": " ++ v
unrollMap input = concat $ intersperse "\n" $ map unrollEntry $ M.assocs input

unroll :: [DebInfo] -> String
unroll input = (concat $ intersperse "\n\n" $ map unrollMap input) ++ "\n"

pkgControl pooldir arch pkg = do
  let path = pooldir ++ "/" ++ arch ++ "/" ++ pkg ++ "/control"
  control_data <- readFile path
  return (read control_data :: DebInfo)

archRelease pooldir arch = do
  let path = pooldir ++ "/" ++ arch
  entries <- getDirectoryContents path
  let pkgs = filter ((/= '.') . head) entries
  controls <- mapM (pkgControl pooldir arch) pkgs
  return (M.fromList $ zip pkgs controls)

loadRelease pooldir = do
  entries <- getDirectoryContents pooldir
  let archs  = filter ((/= '.') . head) entries
  arch_releases <- mapM (archRelease pooldir) archs
  return (M.fromList $ zip archs arch_releases)

same_arch x y = (fst x) == (fst y)

releaseByArch archs debinfo =
  if arch == "all" then
    zip archs (repeat debinfo)
  else
    [(arch, debinfo)]
  where arch = debinfo M.! "Architecture"

releaseDescr (_,deb) = (package, deb) where
  package = deb M.! "Package"

releaseMap debs = (fst $ head $ debs,
                   M.fromList $ map releaseDescr debs)

releaseFrom archs debs = M.fromList release_map where
  all_debs = concatMap (releaseByArch archs) debs
  sorted = sortBy (compare `on` fst) all_debs
  by_arch = groupBy same_arch sorted
  release_map = map releaseMap by_arch

mergeRelease new old arch =
  M.union new_arch old_arch where
    new_arch = case (M.lookup arch new) of
      Nothing -> M.fromList []
      Just x -> x
    old_arch = case (M.lookup arch old) of
      Nothing -> M.fromList []
      Just x -> x

updateRelease archs old new = M.fromList (zip archs updated) where
  updated = map (mergeRelease new old) archs

getPkg :: ApotikiConfig -> (String, ArchRelease) -> (String, B.ByteString)
getPkg config (arch, release) = (relpath, pack str_data) where
  distdir = (configDistDir config)
  component = (configComponent config)
  relname = (configRelease config)
  origin = (configOrigin config)
  label = (configLabel config)
  path = concat $ intersperse "/"
         [distdir, relname, component,
          ("binary-" ++ arch), "Packages"]
  relpath = concat $ intersperse "/"
            [component, ("binary-" ++ arch), "Packages"]
  str_data = unroll $ map snd $ M.assocs release

writePackages :: ApotikiConfig -> (String, B.ByteString) -> IO (String, [String])
writePackages config (relpath, payload) = do
  let path = concat $ intersperse "/" [(configDistDir config),
                                       (configRelease config),
                                       relpath]
  B.writeFile path payload
  return (relpath, flatinfo $ fileinfo payload)

writeGzPackages :: ApotikiConfig -> (String, B.ByteString) -> IO (String, [String])
writeGzPackages config (relpath, payload) = do
  let path = concat $ intersperse "/" [(configDistDir config),
                                       (configRelease config),
                                       relpath ++ ".gz"]
  let gzpayload = B.concat $ BL.toChunks $ Z.compress $ BL.fromChunks [payload]
  B.writeFile path gzpayload
  return (relpath ++ ".gz", flatinfo $ fileinfo gzpayload)

writeArchRelease :: ApotikiConfig -> (String, ArchRelease) -> IO (String, [String])
writeArchRelease config (arch,release) = do
  let distdir = (configDistDir config)
  let component = (configComponent config)
  let relname = (configRelease config)
  let origin = (configOrigin config)
  let label = (configLabel config)
  let path = concat $ intersperse "/"
             [distdir, relname, component,
              ("binary-" ++ arch), "Release"]
  let relpath = concat $ intersperse "/"
                [component, ("binary-" ++ arch), "Release"]

  let payload = pack $ unroll [M.fromList [("Archive", relname),
                                           ("Component", component),
                                           ("Origin", origin),
                                           ("Label", label),
                                           ("Architecture", arch)]]
  B.writeFile path payload
  return (relpath, flatinfo $ fileinfo payload)

md5info (path, [size, sum, _, _]) = " " ++ sum ++ " " ++ size ++ " " ++ path
sha1info (path, [size, _, sum, _]) = " " ++ sum ++ " " ++ size ++ " " ++ path
sha256info (path, [size, _, _, sum]) = " " ++ sum ++ " " ++ size ++ " " ++ path

writeGlobalRelease :: ApotikiConfig -> [(String, [String])] -> IO ()
writeGlobalRelease config info = do
  let archs = concat $ intersperse " " (configArchs config)
  let origin = configOrigin config
  let label = configLabel config
  let release = configRelease config
  let component = configComponent config
  let md5s = concat $ intersperse "\n" $ map md5info info
  let sha1s = concat $ intersperse "\n" $ map sha1info info
  let sha256s = concat $ intersperse "\n" $ map sha256info info
  let sums = concat $ intersperse "\n" $ ["MD5Sum:",
                                          md5s,
                                          "SHA1Sum:",
                                          sha1s,
                                          "SHA256Sum:",
                                          sha256s]
  let summary = concat $ intersperse "\n" ["Origin: " ++ origin,
                                           "Label: " ++ label,
                                           "Suite: " ++ release,
                                           "Codename: " ++ release,
                                           "Components: " ++ component,
                                           "Architectures: " ++ archs]
  let payload = pack $ summary ++ "\n" ++ sums ++ "\n"
  let path = concat $ intersperse "/" [(configDistDir config),
                                       (configRelease config),
                                       "Release"]

  (keys, (time, rng)) <- get_key (configKeyPath config)
  let pgp = sign_msg keys time rng payload
  B.writeFile (path ++ ".gpg") pgp
  B.writeFile path payload


releaseMkDir distdir release component arch =
  createDirectoryIfMissing True $ concat $ intersperse "/" [distdir,
                                                            release,
                                                            component,
                                                            "binary-" ++ arch]
releaseMkDirs ApotikiConfig {distDir = distdir,
                             architectures = archs,
                             release = release,
                             component = component} = do
  mapM_ (releaseMkDir distdir release component) archs

writeRelease :: ApotikiConfig -> Release -> IO ()
writeRelease config release = do
  releaseMkDirs config
  let pkgs = map (getPkg config) (M.assocs release)
  release_files <- mapM (writeArchRelease config) (M.assocs release)
  putStrLn $ "wrote release files: " ++ (show $ length release_files)
  pkg_files <- mapM (writePackages config) pkgs
  putStrLn $ "wrote package files: " ++ (show $ length pkg_files)
  pkg_gz_files <- mapM (writeGzPackages config) pkgs
  putStrLn $ "wrote package compressed files: " ++ (show $ length pkg_gz_files)
  writeGlobalRelease config $ concat [release_files, pkg_files, pkg_gz_files]
