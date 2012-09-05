import Data.List
import Data.Ratio
import Data.Bits

recur::Integer ->Integer
recur n | (%) ( truncate.logBase 2.fromIntegral $ n ) ( n - 1 ) < (%) 1 12345 = n 
	 | otherwise = recur ( n + 1 )

solve::Integer
solve = x^2 - x where 
	x = recur 2

main = print solve

