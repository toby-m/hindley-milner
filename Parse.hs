module Parse ( readExpr )
where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Expression
import Control.Monad

symbol :: Parser Char
symbol = oneOf "+-/%*!?<>-="

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser Expression
parseBool = do char '#'
               x <- oneOf "tf"
               return (Literal . LBool . (== 't') $ x)

parseChar :: Parser Expression
parseChar = do char '\''
               x <- anyChar
               char '\''
               return (Literal . LChar $ x)

parseString :: Parser Expression
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return (Literal . LString $ x)

parseInt :: Parser Expression
parseInt = liftM (Literal . LInt . read) $ many1 digit

parseVariable :: Parser Expression
parseVariable = do first <- letter <|> symbol
                   rest <- many (letter <|> digit <|> symbol)
                   return $ Variable (first:rest)

parseExpr :: Parser Expression
parseExpr = parseInt
        <|> parseChar
        <|> parseBool
        <|> parseVariable
        <|> try parseLambda
        <|> try parseIf
        <|> try parseBlock
        <|> try parseBind
        <|> do char '('
               x <- parseApp
               char ')'
               return x

parseLambda :: Parser Expression
parseLambda = do string "(lambda"
                 spaces
                 char '('
                 args <- sepBy parseVariable spaces
                 char ')'
                 spaces
                 expr <- parseExpr
                 char ')'
                 let vars = map (\(Variable i) -> i) args
                 return $ foldr Abstraction expr vars

parseIf :: Parser Expression
parseIf = do string "(if"
             spaces
             cond <- parseExpr
             spaces
             t <- parseExpr
             spaces
             f <- parseExpr
             char ')'
             return $ If cond t f

parseBlock :: Parser Expression
parseBlock = do string "(block"
                spaces
                body <- sepBy parseExpr spaces
                char ')'
                return $ Block body

parseBind :: Parser Expression
parseBind = do string "(let"
               spaces
               Variable sym <- parseVariable
               spaces
               expr <- parseExpr
               spaces
               expr2 <- parseExpr
               char ')'
               return $ Let sym expr expr2

parseApp :: Parser Expression
parseApp = do f <- parseExpr
              spaces
              arg <- parseExpr
              return $ Application f arg

readExpr :: String -> Expression
readExpr input = case parse parseExpr "scheme" input of
  Left err -> error $ "No match: " ++ show err
  Right val -> val
