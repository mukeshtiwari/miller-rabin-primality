import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T


data AExpr = I Integer | S String | Neg AExpr | A ABinOp AExpr AExpr | F  String [ AExpr ]  deriving ( Show ) 

data ABinOp = Add | Sub | Mul | Div | Pow deriving ( Show ) 

data BExpr = Bool Bool | Not BExpr | B BBinOp BExpr BExpr deriving ( Show ) 

data BBinOp = And | Or deriving ( Show )

data Stmt = Block [ Stmt ] | Assign String AExpr | Switch BExpr Stmt | DoWhile Stmt BExpr | While BExpr Stmt | IfElse BExpr Stmt Stmt | T String AExpr deriving ( Show )


languageDef =
    emptyDef { T.commentStart = "/*" ,
               T.commentEnd = "*/" ,
               T.commentLine = "//" ,
               T.identStart = ( letter <|> char '_' ) ,
               T.identLetter = alphaNum ,
               T.reservedNames = [ "if" , "else" , "while" , "true" , "false" , "for" , "switch" , "do"  ] ,
               T.reservedOpNames = [ "+" , "-" , "*" , "/" , "=" , "==" , "&&" , "||" , "!" , "{" , "}" , "[" , "]" , "(" , ")" ]
             }


lexer = T.makeTokenParser languageDef

identifier = T.identifier lexer
reserved = T.reserved lexer
operator = T.operator lexer
reservedOp = T.reservedOp lexer
charLiteral = T.charLiteral lexer
stringLiteral = T.stringLiteral lexer
natural = T.natural lexer
integer = T.integer lexer
float = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
decimal = T.decimal lexer
hexadecimal = T.hexadecimal lexer
octal = T.octal lexer
symbol = T.symbol lexer
lexeme = T.lexeme lexer
whiteSpace = T.whiteSpace lexer
parens = T.parens lexer
braces = T.braces lexer
angles = T.angles lexer
brackets = T.brackets lexer
semi = T.semi lexer
comma = T.comma lexer
colon = T.colon lexer
dot = T.dot lexer
semiSep = T.semiSep lexer
semiSep1 = T.semiSep1 lexer
commaSep = T.commaSep lexer
commaSep1 = T.commaSep1 lexer





statement :: Parser Stmt 
statement = braces statement <|> sequenceOfStmt  <?> "statement"

sequenceOfStmt :: Parser Stmt 
sequenceOfStmt = do 
          whiteSpace
          list <- semiSep statement'  
          return $ Block list

statement' :: Parser Stmt 
statement' = assignStmt  <|>  ifElseStmt <|> whileStmt  {--<|> doWhileStmt --} <?> "more cases"


rest = number <|> functionStmt <?> "more calls"

number = liftM I integer   


functionStmt = do 
              whiteSpace
              var <- identifier 
              e <- parens . commaSep $ rest
              return $   F var  e


ifElseStmt :: Parser Stmt 
ifElseStmt = do 
             whiteSpace
             reserved "if"
             whiteSpace
             reservedOp "("
             whiteSpace 
             cond <- bExpression 
             whiteSpace
             reservedOp ")"
             whiteSpace 
             reservedOp "{"
             whiteSpace
             stmt_1 <- statement 
             whiteSpace
             reservedOp "}"
             whiteSpace 
             reserved "else"
             whiteSpace
             reservedOp "{"
             whiteSpace
             stmt_2 <- statement 
             whiteSpace 
             reservedOp "}"
             return $ IfElse cond stmt_1 stmt_2 


whileStmt :: Parser Stmt 
whileStmt = do 
            whiteSpace
            reserved "while"
            whiteSpace 
            reservedOp "("
            whiteSpace
            cond <- bExpression 
            whiteSpace 
            reservedOp ")"
            whiteSpace
            reservedOp "{"
            whiteSpace 
            stmt <- statement 
            whiteSpace
            reservedOp "}"
            return $ While cond stmt 

 
assignStmt :: Parser Stmt 
assignStmt = do 
            whiteSpace
            var <- identifier 
            whiteSpace
            reservedOp "=" 
            expr <- ( aExpression  <|> functionStmt )
            return $ Assign var expr



aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [ Prefix ( reservedOp "-" >> return ( Neg  ) ) ] ,
               [ Infix ( reservedOp "*" >> return ( A Mul  ) ) AssocLeft ] ,
               [ Infix ( reservedOp "/" >> return ( A Div ) ) AssocLeft ] ,
               [ Infix ( reservedOp "+" >> return ( A Add  ) ) AssocLeft ] ,
               [ Infix ( reservedOp "-" >> return ( A Sub ) ) AssocLeft ]
             ]

bOperators = [ [ Prefix ( reservedOp "!" >> return ( Not ) ) ] ,
               [ Infix  ( reservedOp "&&" >> return ( B And ) ) AssocLeft ] ,
               [ Infix  ( reservedOp "||" >> return ( B Or ) ) AssocLeft ]
             ]

aTerm = parens aExpression <|> liftM S identifier <|> liftM I integer

bTerm = parens bExpression <|> ( reserved "true" >> return ( Bool True ) )
                           <|> ( reserved "false" >> return  ( Bool False ) )
