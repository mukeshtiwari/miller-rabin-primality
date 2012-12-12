import Data.Maybe ( fromJust )
import qualified Data.ByteString.Lazy.Char8 as BS

powM :: Integer -> Integer -> Integer -> Integer
powM a n m   -- a^n mod m
  | n == 0 = 1 
  | n == 1 = mod a m 
  | even n = ret 
  | otherwise = mod ( a * ret ) m 
  where 
     ret = mod ( powM ( mod ( a ^ 2 ) m ) ( div n 2 ) m ) m

geoSum :: Integer -> Integer -> Integer -> Integer
geoSum r n m 
  | n == 0 = mod 1 m
  | n == 1 = mod ( 1 + r ) m 
  | odd n = mod ( ( 1 + powM r ( div ( n + 1 ) 2 ) m ) * mod ( geoSum r ( div ( n - 1 ) 2 ) m ) m ) m  
  | otherwise = mod (  ( 1 + powM r ( div n 2 ) m ) * mod ( geoSum r ( div ( n - 1 ) 2 ) m ) m  +  powM r n m ) m

solve :: [ Integer ] -> [ BS.ByteString ]
solve [] = []
solve ( a : d : r : n : m : xs )  
   | even n = ( BS.pack . show  $ ret )  :  solve xs 
   | otherwise = (  BS.pack . show $ ( mod ( ret - d + m ) m ) ) :  solve xs 
   where ret  =  mod ( mod ( a * powM r ( div ( n - 1 ) 2 ) m ) m  + mod ( d * geoSum r ( div ( n - 1 ) 2 ) m ) m ) m  

readD :: BS.ByteString -> Integer 
readD = fst . fromJust. BS.readInteger


main =  BS.interact $   BS.unlines . solve . map readD . concat . map BS.words . tail . BS.lines
