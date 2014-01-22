module Main where
import Apotiki.Debian.Package
import Apotiki.Debian.Release
import Apotiki.Config
import Data.Map (keys)
import System.Environment
import Control.Exception
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString as B

main :: IO ()
main = do
  -- first fetch our config
  result <-  tryJust (guard . isDoesNotExistError) $ getEnv "APOTIKI_CONFIG"
  let confpath = case result of
        Left e -> "/etc/apotiki.conf"
        Right val -> val
  confdata <- readFile confpath
  let config = read confdata :: ApotikiConfig
  putStrLn "read config"

  -- now load our view of the world
  old_release <- loadRelease $ configPoolDir config
  putStrLn $ "got previous release: "  ++ (show $ length $ keys old_release)

  -- process new artifacts from command line
  debfiles <- getArgs
  contents <- mapM B.readFile debfiles
  let debinfo = map (debInfo config) contents
  let archs = configArchs config
  let pending_release = releaseFrom archs debinfo

  putStrLn $ "got pending release: "  ++ (show $ length $ keys pending_release)

  -- merge old and new release
  let release = updateRelease archs old_release pending_release

  writeRelease config release

  -- write package to their destination
  mapM_ (writeToPool $ configPoolDir config) $ zip debinfo contents

  putStrLn "done updating repository"
