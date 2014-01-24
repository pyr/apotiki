module System.Apotiki.Config where

data ApotikiConfig = ApotikiConfig {
  keyPath :: String,
  architectures :: [String],
  component :: String,
  release :: String,
  label   :: String,
  origin  :: String,
  repoDir :: String
  } deriving (Show, Read)

configKeyPath ApotikiConfig { keyPath = x } = x
configArchs ApotikiConfig { architectures = x } = x
configComponent ApotikiConfig { component = x } = x
configRelease ApotikiConfig { release = x } = x
configPoolDir ApotikiConfig { repoDir = x } = x ++ "/pool"
configDistDir ApotikiConfig { repoDir = x } = x ++ "/dists"
configRepoDir ApotikiConfig { repoDir = x } = x
configOrigin ApotikiConfig { origin = x } = x
configLabel ApotikiConfig { label = x } = x
