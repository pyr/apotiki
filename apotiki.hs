{-# LANGUAGE OverloadedStrings #-}

module Main where
import Apotiki.Debian.Package
import Apotiki.Debian.Release
import Apotiki.Config
import Data.Map (keys)
import System.Environment
import System.Directory
import Control.Exception

import Network.HTTP.Types.Status
import Web.Scotty
import Data.Text (pack)
import Data.Text.Lazy (unpack)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse

import Data.Aeson (object, (.=))
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B

import Network.Wai.Middleware.Static


main :: IO ()
main = do
  -- first fetch our config
  result <-  tryJust (guard . isDoesNotExistError) $ getEnv "APOTIKI_CONFIG"
  let confpath = case result of
        Left e -> "/etc/apotiki.conf"
        Right val -> val
  confdata <- readFile confpath
  let config = read confdata :: ApotikiConfig
  command:debfiles <- getArgs
  runCommand command config debfiles

runCommand "web" config _ = do

  scotty 8000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/repo" $ do
      repo <- liftIO (releaseJSON $ configPoolDir config)
      json repo
    post "/repo" $ do
      debfiles <- files
      json $ object $ [ "status" .= ("repository updated" :: String)]

runCommand "insert" config debfiles = insertPackages config debfiles

insertPackages config debfiles = do
  createDirectoryIfMissing True (configDistDir config)
  createDirectoryIfMissing True (configPoolDir config)

  -- now load our view of the world
  old_release <- loadRelease $ configPoolDir config
  putStrLn $ "got previous release: "  ++ (show $ length $ keys old_release)

  -- process new artifacts from command line
  contents <- mapM B.readFile debfiles
  let debinfo = map (debInfo config) contents
  let archs = configArchs config
  let pending_release = releaseFrom archs debinfo

  putStrLn $ "got pending release: "  ++ (show $ length $ keys pending_release)

  -- merge old and new release
  let release = updateRelease archs old_release pending_release

  writeRelease config release

  -- write package to their destination
  mapM_ (writeToPool $ configRepoDir config) $ zip debinfo contents

  putStrLn "done updating repository"
