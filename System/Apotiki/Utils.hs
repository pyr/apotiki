module System.Apotiki.Utils (strip) where
import Data.List

-- inspired by MissingH

wschars :: String
wschars = " \t\r\n"

is_white :: Char -> Bool
is_white c = isInfixOf [c] wschars

lstrip :: String -> String
lstrip s = dropWhile is_white s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip
