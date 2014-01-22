module Apotiki.Config where

data ApotikiConfig = ApotikiConfig {
  keyPath :: String,
  architectures :: [String],
  component :: String,
  release :: String,
  label   :: String,
  origin  :: String,
  poolDir :: String,
  distDir :: String
  } deriving (Show, Read)

configKeyPath ApotikiConfig { keyPath = x } = x
configArchs ApotikiConfig { architectures = x } = x
configComponent ApotikiConfig { component = x } = x
configRelease ApotikiConfig { release = x } = x
configPoolDir ApotikiConfig { poolDir = x } = x
configDistDir ApotikiConfig { distDir = x } = x
configOrigin ApotikiConfig { origin = x } = x
configLabel ApotikiConfig { label = x } = x
