module Apotiki.Debian.Release where
import Apotiki.Debian.Package
import System.Directory
import Data.List
import Data.Function
import qualified Data.Map as M

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


writeRelease path release = do
  writeFile path $ show release
