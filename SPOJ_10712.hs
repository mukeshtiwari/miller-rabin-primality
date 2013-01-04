import Data.ByteString.Lazy.Char8 as BS hiding  ( map , tail , filter  , null )
import Data.Maybe ( fromJust )
import Text.Printf ( printf ) 

diffEQ :: Double -> Double -> Double -> Double 
diffEQ a b x = a + b * cos x  

valEQ :: Double -> Double -> Double -> Double -> Double 
valEQ a b c x = a * x   +  b * sin x - c

evalFun :: [ Int ]  -> Double
evalFun [ a' , b' , c' ]  = ret where 
          ( a , b , c  ) = ( fromIntegral a' , fromIntegral b' , fromIntegral c' )
          ret = fst . until ( \ ( _ , cnt ) -> cnt  >=  100 )  ( \( x , cnt ) -> ( x - (  valEQ a b c x  /  diffEQ a b x ) , succ cnt ) ) $ (  c / a  , 0   ) 

readD :: BS.ByteString -> Int
readD = fst . fromJust . BS.readInt 

main = BS.interact $ BS.unlines . map (  BS.pack . ( printf "%.6f" :: Double -> String ) . evalFun .  map readD ) . filter ( not.null ) . map BS.words . tail . BS.lines 

