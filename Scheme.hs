{-# Language GADTs , ExistentialQuantification #-}
import Control.Applicative hiding ( many , ( <|> )  )
import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment 
import Control.Monad.Error
import System.IO hiding  ( try ) 
import Data.IORef 

data LispVal where 
     Atom :: String -> LispVal 
     List :: [ LispVal ] -> LispVal
     DottedList :: [ LispVal ] -> LispVal -> LispVal
     Number :: Integer -> LispVal
     String :: String -> LispVal
     Bool :: Bool -> LispVal
     PrimitiveFunc :: ( [ LispVal ] -> ThrowsError LispVal ) -> LispVal
     Fun :: { params :: [ String ] ,  vararg :: Maybe String , body :: [ LispVal ] ,    closure :: Env }  -> LispVal

data LispError where 
     NumArgs :: Integer -> [ LispVal ] -> LispError
     TypeMismatch :: String -> LispVal -> LispError
     Parser :: ParseError -> LispError
     BadSpecialForm :: String -> LispVal -> LispError
     NotFunction :: String -> String -> LispError
     UnboundVar :: String -> String -> LispError
     Default :: String -> LispError

data Unpacker = forall a.Eq a => AnyUnpacker ( LispVal -> ThrowsError a ) 



instance Show LispVal where 
     show ( Atom name ) = name 
     show ( List contents ) = "( " ++ ( unwords . map show $ contents ) ++ " )"
     show ( DottedList head tail ) = "( " ++ ( unwords . map show $ head ) ++ " . " ++ show tail ++ " )"
     show ( Number contents ) = show contents 
     show ( String contents ) = "\"" ++  contents ++ "\""
     show ( Bool True ) = "#t"
     show ( Bool False ) = "#f" 


instance Show LispError where 
    show (  UnboundVar message varname ) = message ++ ": "++ varname
    show (  BadSpecialForm message form ) = message ++ ": "++ show form 
    show ( NotFunction message func ) = message ++": "++ show func
    show ( NumArgs expected found ) = "Expected "++ show expected ++ " args: found vales " ++ ( unwords . map show $ found )
    show ( TypeMismatch expected found ) = "Invalid types: expected " ++ expected ++ "found " ++ show found 
    show ( Parser parseErr ) = "Parse error at " ++ show parseErr 
     
instance Error LispError where 
   noMsg = Default "An error has occured"
   strMsg = Default


type ThrowsError = Either LispError 

trapError action = catchError action ( return .show ) 

extractValue :: ThrowsError a -> a 
extractValue ( Right val ) = val


symbol :: Parser Char
symbol =  oneOf "!$%&|+-*/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany space 

parseString :: Parser LispVal
parseString = String <$> ( char '"' *>  x <*  char '"' ) where 
            x = many ( noneOf "\"" ) 

parseAtom :: Parser LispVal
parseAtom = do 
       atom <-  ( : ) <$> ( letter <|> symbol ) <*> many ( letter <|> digit <|> symbol )
       return $ case atom of 
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom


parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit


parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr space 

parseDottedList :: Parser LispVal
parseDottedList = DottedList <$> ( endBy parseExpr space ) <*> ( char '.' *> spaces *> parseExpr )


parseQuoted :: Parser LispVal
parseQuoted =  f  <$>   ( char '\'' *> parseExpr ) where 
              f x =   List [ Atom "quote" , x ] 

parseExpr :: Parser LispVal
parseExpr =  parseAtom 
         <|> parseString
         <|> parseNumber 
         <|> parseQuoted
         <|> ( char '(' *> spaces *> ( ( try parseList ) <|> parseDottedList ) <* spaces  <* char ')' )
 

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@( String _ ) = return val
eval env val@( Number _ ) = return val
eval env val@( Bool _ ) = return val
eval env ( Atom id ) = getVar env id
eval env ( List [ Atom "if" , pred , conseq , alt ] )  = 
    do result <- eval env pred 
       case result of 
            Bool True -> eval env conseq
            Bool False -> eval env alt
eval env ( List [ Atom "quote" , val ] ) = return val 
eval env ( List [ Atom "set!" , Atom var , form ] ) = 
     eval env form >>= setVar env var 
eval env ( List [ Atom "define" , Atom var , form ] ) = 
     eval env form >>= defineVar env var
eval env ( List ( Atom func : args ) ) =  mapM ( eval env ) args >>= 
           liftThrows . apply func 
eval env badForm = throwError $  BadSpecialForm  "Unrecognised special form" badForm 


car :: [ LispVal ] -> ThrowsError LispVal
car [ List ( x : xs ) ] = return x
car [ DottedList ( x : xs ) _ ] = return x
car [ badArgs ] = throwError . TypeMismatch "pair" $ badArgs
car badArgsList = throwError . NumArgs 1 $ badArgsList

cdr :: [ LispVal ] -> ThrowsError LispVal 
cdr [ List ( x : xs ) ] = return . List $ xs 
cdr [ DottedList [xs] x ] = return x
cdr [ DottedList ( _ : xs ) x ] = return . DottedList xs $ x 
cdr [ badArgs ] = throwError . TypeMismatch "pair" $ badArgs 
cdr badArgsList = throwError . NumArgs 1 $ badArgsList

cons :: [ LispVal ] -> ThrowsError LispVal
cons [ x , List [] ] = return . List $ [x] 
cons [ x , List xs ] = return . List $ x : xs 
cons [ x , DottedList xs xlast ] = return . DottedList ( x : xs ) $ xlast 
cons [ x1 , x2 ] = return . DottedList [ x1 ] $ x2 
cons badArgsList = throwError . NumArgs 2 $ badArgsList 

eqv :: [ LispVal ] -> ThrowsError LispVal
eqv [ Bool arg1 , Bool arg2 ] = return . Bool $ arg1 == arg2
eqv [ Number arg1 , Number arg2 ] = return . Bool $ arg1 == arg2 
eqv [ String arg1 , String arg2 ] = return . Bool $ arg1 == arg2 
eqv [ Atom arg1 , Atom arg2 ] = return . Bool $ arg1 == arg2 
eqv [ DottedList xs x , DottedList ys y ] = eqv [ List $ xs ++ [ x ] , List $ ys ++ [y] ]
eqv [ List xs , List ys ] = return . Bool $ ( length xs == length ys ) &&  
           ( and . map eqvPair . zip xs $ ys ) where 
            eqvPair ( x , y ) = case eqv [ x , y ] of 
                                     Left err -> False
                                     Right ( Bool val ) -> val
eqv [ _ , _ ] = return . Bool $ False 
eqv badArgList = throwError . NumArgs 2 $ badArgList


apply :: String -> [ LispVal ] -> ThrowsError LispVal
apply func args = maybe ( throwError . NotFunction "Unrecognised primitive function arguments " $ func  ) ( $ args ) . lookup func $ primitives 


primitives :: [ ( String , [ LispVal ] -> ThrowsError LispVal ) ]
primitives = [ ( "+" , numericBinop ( + ) ) ,
               ( "-" , numericBinop ( - ) ) ,
               ( "*" , numericBinop ( * ) ) ,
               ( "/" , numericBinop div ) ,
               ( "mod" , numericBinop mod ) ,
               ( "quotient" , numericBinop quot ),
               ( "remainder" , numericBinop rem ),
               ( "=" , numBoolBinop ( == ) ),
               ( "<" , numBoolBinop ( < ) ),
               ( ">" , numBoolBinop ( > ) ), 
               ( "/=" , numBoolBinop ( /= ) ),
               ( ">=" , numBoolBinop ( >= ) ),
               ( "<=" , numBoolBinop ( <= ) ),
               ( "&&" , boolBoolBinop ( && ) ),
               ( "||" , boolBoolBinop ( || ) ),
               ( "string=?" , strBoolBinop ( == ) ),
               ( "string?" , strBoolBinop ( > ) ),
               ( "string<=?" , strBoolBinop ( <= ) ),
               ( "string>=?" , strBoolBinop ( >= ) ),
               ( "car" , car ),
               ( "cdr" , cdr ),
               ( "cons" , cons ),
               ( "eq?" , eqv ),
               ( "eqv?" , eqv ),
               ( "equal?" , equal ) ]

boolBinop :: ( LispVal -> ThrowsError a ) -> ( a -> a -> Bool ) -> [ LispVal ] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                              then throwError $ NumArgs 2 args 
                             else do
                                  left <- unpacker  ( args !! 0 ) 
                                  right <- unpacker ( args !! 1 )
                                  return . Bool $ left `op` right 

numBoolBinop = boolBinop unpackNum 
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr ( String s ) = return s
unpackStr ( Number s ) = return . show $ s
unpackStr ( Bool s ) = return . show $ s 
unpackStr notString = throwError . TypeMismatch "string" $ notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool ( Bool b ) = return b
unpackBool notBool = throwError . TypeMismatch "boolean" $ notBool  
                               
numericBinop :: ( Integer -> Integer -> Integer ) -> [ LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError . NumArgs 2 $ singleVal
numericBinop op params =  mapM unpackNum params >>= return . Number . foldl1  op 

unpackNum :: LispVal -> ThrowsError Integer
unpackNum ( Number n ) = return n 
unpackNum ( String n ) = if null parsed 
                          then throwError . TypeMismatch "number" $ String n 
                         else return . fst $ ( parsed !! 0 ) where 
                         parsed = reads n
unpackNum ( List [ n ] ) = unpackNum n
unpackNum notNum  = throwError . TypeMismatch "number" $ notNum  

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 ( AnyUnpacker unpacker ) = do 
             unpacked1 <- unpacker arg1 
             unpacked2 <- unpacker arg2 
             return $ unpacked1 == unpacked2 
         `catchError` ( const . return $ False ) 

equal :: [ LispVal ] -> ThrowsError LispVal
equal [ arg1 , arg2 ] = do 
      primitiveEquals <- liftM or . mapM ( unpackEquals arg1 arg2 ) 
         $ [ AnyUnpacker unpackNum , AnyUnpacker unpackStr , AnyUnpacker unpackBool]
      eqvEquals <- eqv [ arg1 , arg2 ]
      return . Bool $ ( primitiveEquals || let ( Bool x ) = eqvEquals in x )
equal badArgList = throwError . NumArgs 2 $ badArgList 


readExpr :: String -> ThrowsError LispVal  
readExpr input = case parse ( spaces *> parseExpr <* spaces )  "lisp" input of 
        Left err -> throwError . Parser $ err
        Right val ->  return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String 
readPrompt  prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String 
evalString env expr = runIOThrows . liftM show $ ( liftThrows . readExpr $ expr ) >>=
               eval env 

runOne :: String -> IO()
runOne expr = nullEnv >>= flip evalAndPrint expr 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint  env expr = evalString env expr >>= putStrLn 

until_ :: Monad m => ( a -> Bool ) -> m a -> ( a -> m () ) -> m ()
until_ pred prompt action = do 
       result <- prompt 
       if pred result 
        then return ()
       else action result >> until_ pred prompt action 

runRepl :: IO ()
runRepl = nullEnv >>= until_ ( == "quit" ) ( readPrompt "Lisp>>> " ) . evalAndPrint  

type Env = IORef [ ( String , IORef LispVal ) ]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO 

liftThrows :: ThrowsError a -> IOThrowsError a 
liftThrows ( Left err ) = throwError err
liftThrows ( Right val ) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT ( trapError action ) >>= return . extractValue 

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False ( const True ) 
        . lookup var 

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO . readIORef $ envRef
                       maybe ( throwError . UnboundVar "Getting an unbound variable" 
                              $ var ) ( liftIO . readIORef ) ( lookup var env ) 

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
       env <- liftIO . readIORef $ envRef
       maybe ( throwError . UnboundVar "Setting an unbound variable" $ var ) 
               ( liftIO . ( flip writeIORef value ) ) ( lookup var env ) 
       return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal 
defineVar envRef var value = do 
       alreadyDefined <- liftIO . isBound envRef $ var
       case alreadyDefined of 
            True -> setVar envRef var value >> return value
            False -> liftIO $ do 
                  valueRef <- newIORef value
                  env <- readIORef envRef 
                  writeIORef envRef ( ( var , valueRef ) : env )
                  return value
                 
bindVars :: Env -> [ ( String , LispVal )] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef 
         where extendEnv bindings env = liftM ( ++ env ) ( mapM addBinding bindings )
               addBinding ( var , value ) = do ref <- newIORef value
                                               return ( var , ref )  


 
main :: IO ()
main = do 
   args <- getArgs 
   case length args of 
        0 -> runRepl 
        1 -> runOne  ( args !! 0 ) 
        _ -> putStrLn "Program takes only 0 or 1 arguments"
   