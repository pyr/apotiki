module Apotiki.Config where

data ApotikiConfig = ApotikiConfig {
  keyPath :: String,
  architectures :: [String],
  component :: String,
  release :: String,
  poolDir :: String,
  distDir :: String
  } deriving (Show, Read)

defaultConfig = ApotikiConfig {
  keyPath = "/home/pyr/.gnupg/ops.key",
  architectures = ["amd64"],
  component = "main",
  release = "precise",
  poolDir = "/tmp/repo/pool",
  distDir = "/tmp/repo/dist"
  }

configKeyPath ApotikiConfig { keyPath = x } = x
configArchs ApotikiConfig { architectures = x } = x
configComponent ApotikiConfig { component = x } = x
configRelease ApotikiConfig { release = x } = x
configPoolDir ApotikiConfig { poolDir = x } = x
configDistDir ApotikiConfig { distDir = x } = x
