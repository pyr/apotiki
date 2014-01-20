module Apotiki.Signature (sign_data, get_key) where
import System.Time (getClockTime, ClockTime(..))
import Crypto.Random
import Data.OpenPGP
import qualified Data.Binary as Binary
import qualified Data.OpenPGP.CryptoAPI as PGP
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU
import Codec.Encryption.OpenPGP.ASCIIArmor as Armor
import Codec.Encryption.OpenPGP.ASCIIArmor.Types as Armor

get_key keypath = do
  keys <- Binary.decodeFile keypath
  TOD tod _ <- getClockTime
  rng <- newGenIO :: IO SystemRandom
  let time = (fromIntegral tod :: Integer)
  return (keys, (time, rng))

sign_msg :: (CryptoRandomGen g) => Message -> Integer -> g -> BU.ByteString -> String
sign_msg  keys time rng payload =
  BS.unpack $ Armor.encode [armor] where
    wtime = (fromIntegral time :: Binary.Word32)
    pgp_payload = LiteralDataPacket 'b' "" wtime payload
    input = (DataSignature pgp_payload [])
    (DataSignature _ [sig], _) = PGP.sign keys input SHA256 [] time rng
    options = [("Version", "OpenPrivacy 0.99"), ("Hash", "SHA256")]
    encoded_sig = Binary.encode sig
    armor = Armor.Armor Armor.ArmorSignature options encoded_sig


test2 keypath path = do
  (keys, (time, rng)) <- get_key keypath
  payload <- B.readFile path
  let armor = sign_msg keys time rng payload
  putStrLn $ armor

test keypath path = do
  time <- getClockTime
  rng <- newGenIO :: IO SystemRandom
  let TOD t _ = time
  let timestamp = (fromIntegral t)
  keys <- Binary.decodeFile keypath
  content <- B.readFile path
  let pgp_payload = LiteralDataPacket 'b' "" (fromIntegral t) content
  let input = (DataSignature pgp_payload [])
  let (DataSignature _ [sig], _) = PGP.sign keys input SHA256 [] timestamp rng
  let options = [("Version", "OpenPrivacy 0.99"), ("Hash", "SHA256")]
  let encoded_sig = Binary.encode sig
  let armor = Armor.Armor Armor.ArmorSignature options encoded_sig
  return (BS.unpack $ Armor.encode [armor])
