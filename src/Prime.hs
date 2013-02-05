module Prime ( primeRange , isPrime ) where 

import Data.Bits
import Control.Parallel.Strategies 

powM :: Integer -> Integer -> Integer ->  Integer
powM a d n = powM' a d n where 
  powM' a d n 
        | d == 0 = 1
        | d == 1 = mod a n
        | otherwise = mod q n  where
	       p = powM'  ( mod ( a^2 ) n ) ( shiftR d 1 ) n
	       q = if (.&.) d 1 == 1 then mod ( a * p ) n else p

calSd :: Integer -> ( Integer , Integer ) 
calSd n = ( s , d ) where 
      s = until ( \x -> testBit ( n - 1) ( fromIntegral x ) )  ( +1 ) 0
      d = div ( n - 1 ) (  shiftL 1 ( fromIntegral s )  )


rabinMiller::Integer->Integer->Integer->Integer-> Bool
rabinMiller  n s d a 
   | n == a = True 
   | otherwise =  case x == 1 of 
          True -> True 
          _ ->   any ( == pred n ) . take ( fromIntegral s ) 
                      . iterate (\e -> mod ( e^2 ) n ) $ x  
        where 
              x = powM a d n 
   

isPrime::Integer-> Bool
isPrime n 
   | n <= 1 = False
   | n == 2 = True
   | even n = False
   | otherwise	= all ( == True ) . map ( rabinMiller n s d ) $  [ 2 , 3 , 5 , 7 , 11 , 13 , 17 ] where 
                ( s , d ) = calSd n 



primeRange :: Integer -> Integer -> [ Bool ]
primeRange m n = ( map isPrime [ m .. n ] ) `using` parListChunk 10000 rdeepseq 

