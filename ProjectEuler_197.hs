import Data.List

f :: Double -> Double
f x = 1E-9 * ret where
   a = 30.403243784
   y = a - x ^ 2
   ret = fromIntegral ( floor $ 2 ** y )

recur :: [ Double ] -> Double
recur ( a : b : c : xs )
   | abs ( a - c ) <= 1E-10   = a + b
   | otherwise = recur ( b : c : xs )

solve :: Double
solve = recur . iterate  f $ ( - 1 )

main :: IO ()
main = print $ solve  

