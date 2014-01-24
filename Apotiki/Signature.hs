module Apotiki.Signature (sign_msg, get_key) where
import System.Time (getClockTime, ClockTime(..))
import Crypto.Random
import Data.OpenPGP
import qualified Data.Binary as Binary
import qualified Data.OpenPGP.CryptoAPI as PGP
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Codec.Encryption.OpenPGP.ASCIIArmor as Armor
import Codec.Encryption.OpenPGP.ASCIIArmor.Types as Armor

get_key keypath = do
  keys <- Binary.decodeFile keypath
  TOD tod _ <- getClockTime
  rng <- newGenIO :: IO SystemRandom
  let time = (fromIntegral tod :: Integer)
  return (keys, (time, rng))

sign_msg :: (CryptoRandomGen g) => Message -> Integer -> g -> BS.ByteString -> BS.ByteString
sign_msg  keys time rng payload =
  Armor.encode [armor] where
    wtime = (fromIntegral time :: Binary.Word32)
    lazy_payload = B.fromChunks [payload]
    pgp_payload = LiteralDataPacket 'b' "" wtime lazy_payload
    input = (DataSignature pgp_payload [])
    (DataSignature _ [sig], _) = PGP.sign keys input SHA256 [] time rng
    options = [("Version", "OpenPrivacy 0.99"), ("Hash", "SHA256")]
    encoded_sig = Binary.encode sig
    armor = Armor.Armor Armor.ArmorSignature options encoded_sig
