{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Apotiki.Debian.Package
import System.Apotiki.Debian.Release
import System.Apotiki.Config
import System.Apotiki.Templates
import System.Apotiki.Logger
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
import qualified Data.Text.Lazy as T

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
  config <- parseFile confpath
  logger <- log_start (configLogPath config)
  log_info logger "starting up"
  args <- getArgs
  runCommand logger config args

runCommand logger config [] = runCommand logger config ["help"]

runCommand logger config ("help":debfiles) = do
  putStrLn "usage: apotiki {web, insert} [packages]"

runCommand logger config ("web":_) = do
  setupRepo config
  scotty 8000 $ do
    get "/apotiki.js" $ do
      html $ T.pack jsApp
    get "/index.html" $ do
      html $ T.pack indexHtml
    get "/listing.html" $ do
      html $ T.pack listingHtml
    get "/details.html" $ do
      html $ T.pack detailsHtml
    get "/post.html" $ do
      html $ T.pack postHtml
    get "/" $ do
      redirect "/index.html"
    get "/repo" $ do
      repo <- liftIO (releaseJSON $ configPoolDir config)
      json repo
    post "/repo" $ do
      indata <- files
      let debfiles = [B.concat $ toChunks $ fileContent fi | (_,fi) <- indata]
      liftIO $ insertPackages logger config debfiles
      redirect "/index.html"

runCommand logger config ("insert":filenames) = do
  setupRepo config
  debfiles <- mapM B.readFile filenames
  insertPackages logger config debfiles

insertPackages logger config debfiles = do
  -- now load our view of the world
  old_release <- loadRelease $ configPoolDir config
  log_info logger $ "got previous release: "  ++ (show $ length $ keys old_release)

  let debinfo = map (debInfo config) debfiles
  let archs = configArchs config
  let pending_release = releaseFrom archs debinfo

  log_info logger $ "got pending release: "  ++ (show $ length $ keys pending_release)

  -- merge old and new release
  let release = updateRelease archs old_release pending_release

  writeRelease logger config release

  -- write package to their destination
  mapM_ (writeToPool logger $ configRepoDir config) $ zip debinfo debfiles

  log_info logger "done updating repository"
