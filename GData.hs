module GData where 

data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test
        deriving ( Show ) 


eval :: Expr -> Maybe ( Either Int Bool ) 
eval ( I n )  = Just ( Left n ) 
eval ( B b )  = Just ( Right b )
eval ( Add e1 e2 ) = case ( eval e1 , eval e2 ) of 
                   ( Just ( Left a ) , Just ( Left b ) ) -> Just ( Left ( a + b ) ) 
                   ( Just ( Right b ) , _ ) -> Nothing 
                   ( _ , Just ( Right b ) ) -> Nothing 
            

eval ( Mul e1 e2 ) = case ( eval e1 , eval e2 ) of 
                  ( Just ( Left a ) , Just ( Left b ) ) -> Just ( Left ( a * b ) )
                  ( Just ( Right b ) , _ ) -> Nothing
                  ( _ , Just ( Right b ) ) -> Nothing

eval ( Eq e1 e2 ) = case ( eval e1 , eval e2 ) of
                       ( Just ( Left a )  , Just ( Left b ) )  -> Just ( Right ( a == b ) )
                       ( Just ( Right a ) , Just ( Right b ) ) -> Just ( Right ( a == b ) ) -- this will for single expression like B True `Eq` B True
                       ( _ , _ ) -> Nothing  -- No comparision between Int and Bool 
                       
--main :: IO ()
--main = print.eval $ ( ( I 5 `Add` I 1 ) `Mul` I 7 :: Expr )  `Eq` ( ( I 5 `Add` I 1 ) `Mul` I 7 :: Expr )       
