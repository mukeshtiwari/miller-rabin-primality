import Control.Monad.ST 
import Data.Array.ST 
import Data.Array.Unboxed 
import Control.Monad 
import Data.List


prime :: Int -> UArray Int Bool 
prime n = runSTUArray $ do 
    arr <- newArray ( 2 , n ) True :: ST s ( STUArray s Int Bool ) 
    forM_ ( takeWhile ( \x -> x*x <= n ) [ 2 .. n ] ) $ \i -> do 
        ai <- readArray arr i 
        when ( ai  ) $ forM_ [ i^2 , i^2 + i .. n ] $ \j -> do 
            writeArray arr j False 

    return arr 

pList :: UArray Int Bool 
pList = prime $  10 ^ 8 

divPrime :: Int -> Bool 
divPrime n = all ( \d -> if mod n d == 0 then pList ! ( d + div  n  d ) else True )  $  [ 1 .. truncate . sqrt . fromIntegral  $ n ] 



main = putStrLn . show . sum  $ ( [ if and [ pList ! i , divPrime . pred $ i ] then ( fromIntegral . pred $ i ) else 0 | i <- [ 2 .. 10 ^ 8 ] ] :: [ Integer ] ) 

