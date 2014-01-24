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
import Data.ByteString.Lazy (toChunks)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse

import Data.Aeson (object, (.=))
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B

import Network.Wai.Middleware.Static

setupRepo config = do
  createDirectoryIfMissing True (configDistDir config)
  createDirectoryIfMissing True (configPoolDir config)

main :: IO ()
main = do
  -- first fetch our config
  result <-  tryJust (guard . isDoesNotExistError) $ getEnv "APOTIKI_CONFIG"
  let confpath = case result of
        Left e -> "/etc/apotiki.conf"
        Right val -> val
  confdata <- readFile confpath
  let config = read confdata :: ApotikiConfig
  args <- getArgs
  runCommand config args

runCommand config [] = runCommand config ["help"]

runCommand config ("help":debfiles) = do
  putStrLn "usage: apotiki {web, insert} [packages]"

runCommand config ("web":_) = do
  setupRepo config
  scotty 8000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/repo" $ do
      repo <- liftIO (releaseJSON $ configPoolDir config)
      json repo
    post "/repo" $ do
      indata <- files
      let debfiles = [B.concat $ toChunks $ fileContent fi | (_,fi) <- indata]
      liftIO $ insertPackages config debfiles
      json $ object $ [ "status" .= ("repository updated" :: String)]

runCommand config ("insert":filenames) = do
  setupRepo config
  debfiles <- mapM B.readFile filenames
  insertPackages config debfiles

insertPackages config debfiles = do
  -- now load our view of the world
  old_release <- loadRelease $ configPoolDir config
  putStrLn $ "got previous release: "  ++ (show $ length $ keys old_release)

  let debinfo = map (debInfo config) debfiles
  let archs = configArchs config
  let pending_release = releaseFrom archs debinfo

  putStrLn $ "got pending release: "  ++ (show $ length $ keys pending_release)

  -- merge old and new release
  let release = updateRelease archs old_release pending_release

  writeRelease config release

  -- write package to their destination
  mapM_ (writeToPool $ configRepoDir config) $ zip debinfo debfiles

  putStrLn "done updating repository"
