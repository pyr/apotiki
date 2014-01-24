module System.Apotiki.FileInfo (sha256sum, sha1sum, md5sum, fileinfo) where
import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.MD5 as MD5
import qualified Text.Printf as P
import qualified Data.Map as M

gensum input hashfn = B.unpack (hashfn input) >>= P.printf "%02x"
sha256sum input = gensum input SHA256.hash
sha1sum input = gensum input SHA1.hash
md5sum input = gensum input MD5.hash
fileinfo input = M.fromList [("Size", show $ B.length input),
                             ("MD5sum", md5sum input),
                             ("SHA1", sha1sum input),
                             ("SHA256", sha256sum input)]
