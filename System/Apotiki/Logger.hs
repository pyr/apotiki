module System.Apotiki.Logger (log_start, log_info, LogChan) where
import System.IO (openFile, hPutStrLn, hFlush, hClose,
                  IOMode (AppendMode), Handle)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan, readTChan, newTChanIO, TChan)

-- Simplistic logging module

type LogChan = TChan String

put_line :: String -> String -> IO ()
put_line path line = do
  fd <- openFile path AppendMode
  hPutStrLn fd line
  hFlush fd
  hClose fd

log_line :: TChan String -> String -> IO ()
log_line chan path = do
  line <- atomically $ readTChan chan
  if path == "STDOUT" then (putStrLn line) else (put_line path line)

log_start :: String -> IO (TChan String)
log_start path = do
  chan <- newTChanIO
  void $ forkIO $ forever $ log_line chan path
  return chan

log_info :: TChan String -> String -> IO ()
log_info chan msg = do
  let info_msg = "[info] " ++ msg
  atomically $ writeTChan chan info_msg
