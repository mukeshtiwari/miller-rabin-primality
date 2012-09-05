import Data.List
import qualified Data.IntMap.Lazy as M
import qualified Data.PQueue.Min as PQ
import qualified Data.Bits as B


buildGraph :: [ ( Int , Int , Int ) ]  -> M.IntMap [ ( Int , Int ) ]
buildGraph xs = M.fromListWith ( ++ ) . map f_1  $ xs where
          f_1 ( i , j , val ) = ( i , [ (  val , j ) ] )



visited :: Int -> Integer -> ( Bool , Integer )
visited x vis = ( t == 0 , vis' ) where
            t = ( B..&. ) ( B.shiftL ( 1 :: Integer ) x ) vis
            vis' = ( B..|. ) ( B.shiftL ( 1 :: Integer ) x ) vis


primAlgorithm ::  PQ.MinQueue ( Int , Int )  -> Integer -> Int -> M.IntMap [ ( Int , Int ) ] -> String
primAlgorithm  queue  vis cost m
       | PQ.null queue =  show cost  ++ "\n"
       | t == False = primAlgorithm  queue' vis' cost m
       | otherwise =   primAlgorithm queue'' vis' ( cost + c ) m where 
              ( ( c , x ) , queue' ) = PQ.deleteFindMin queue 
              ( t , vis' ) = visited x vis
              queue'' = PQ.union queue' ( PQ.fromList . ( M.! ) m  $ x )


test :: [ ( Int , Int , Int ) ] -> String
test xs = primAlgorithm  ( PQ.fromList [ ( 0 , 0 ) ] ) 0 0  ( buildGraph xs ) 

parseInput :: [ ( Int , String ) ] -> [ ( Int , Int ) ]
parseInput [] = []
parseInput ( ( k , x )  : xs ) 
   | x == "-" = parseInput xs 
   | otherwise = ( k , readD x ) : parseInput xs where 
        readD :: String -> Int
        readD = read

fun :: Int -> [ [ ( Int , Int ) ] ] -> [ [ ( Int , Int , Int ) ] ]
fun _ [] = []
fun k ( x : xs ) = ( map ( \( i , val ) -> ( k , i , val ) ) x ) : fun ( succ k ) xs 


main =  interact $  primAlgorithm ( PQ.fromList [ ( 0 , 0 ) ] ) 0 0 .  buildGraph . concat . fun 0 . map ( parseInput  . zip [ 0 , 1 .. ] . words ) . lines   

