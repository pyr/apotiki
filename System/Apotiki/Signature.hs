{-# LANGUAGE OverloadedStrings #-}
module System.Apotiki.Signature (sign_msg, get_key) where
import System.Time (getClockTime, ClockTime(..))
import Crypto.Random
import Data.OpenPGP
import Data.String
import qualified Data.Binary as Binary
import qualified Data.OpenPGP.CryptoAPI as PGP
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Codec.Encryption.OpenPGP.ASCIIArmor as Armor
import Codec.Encryption.OpenPGP.ASCIIArmor.Types as Armor

get_key keypath = do
  let payload = BC.pack keypath
  let Right decoded_key = Armor.decode payload
  let ((Armor _ _ bskey):_) = decoded_key
  let key = Binary.decode bskey
  TOD tod _ <- getClockTime
  rng <- newGenIO :: IO SystemRandom
  let time = (fromIntegral tod :: Integer)
  return (key, (time, rng))

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
