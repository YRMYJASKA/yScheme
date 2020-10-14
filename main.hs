-- | Write Yourself a Scheme

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Numeric (readHex, readOct)

-- Acceptable terminal symbols
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Make the parser ignore whitespaces
spaces :: Parser ()
spaces = skipMany1 space


-- How our Scheme should be parsed
data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | Character String
                | Float Float
                deriving (Show)

-- Parsing LispVal

-- Character Name          ASCII Name
--------------          ----------
-- altmode                 ESC
-- backnext                US
-- backspace               BS
-- call                    SUB
-- linefeed                LF
-- page                    FF
-- return                  CR
-- rubout                  DEL
-- space
-- tab                     HT
parseCharacter :: Parser LispVal
parseCharacter = do
                char '#'
                char '\\'
                x <-  alphaNum <|> oneOf "\\()[]\"\'" <|> symbol
                xs <- many letter
                return $ Character $ if (x:xs) `elem` ["altmode", "backnext", "backspace", "call", "linefeed", "page", "return", "rubout", "space", "tab"]
                                then  x:xs
                                else  [x]

parseEscape :: Parser Char
parseEscape = do
                char '\\'
                x <- oneOf "\\\'\"ntr"
                return $ case x of
                           'n' -> '\n'
                           'r' -> '\r'
                           't' -> '\t'
                           _ -> x

parseString :: Parser LispVal
parseString = do
                char '"'
                -- x <- many (noneOf "\"")
                x <- many (parseEscape <|>alphaNum <|> oneOf " " <|> symbol)
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter
                rest <- many (letter <|> digit <|> symbol)
                let atom  = first:rest
                return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom

intOrFloat :: String -> LispVal
intOrFloat s = if '.' `elem` s then Float (read s) else Number (read s)

-- Taken from https://tehgeekmeister.wordpress.com/2008/01/11/one-line-binary-reader-in-haskell/
readBin :: String -> Integer
readBin = (`div` 2) . foldl (\x y -> (x + y) * 2) 0 . map (\c -> case c of {'0' -> 0; '1' -> 1; _ -> error "Input is not a binary string."})


parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
                f <- oneOf "#" <|> digit
                s <- oneOf ".bodx" <|> digit
                x <- many (alphaNum <|> oneOf ".")
                return (case f of
                                '#' -> case s of
                                                'b' -> Number $ readBin  x
                                                'o' -> Number $ fst $ head (readOct x)
                                                'd' -> intOrFloat x
                                                'x' -> Number $ fst $ head (readHex x )
                                _ -> intOrFloat (f:s:x))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- Finally, put all the parsers into one
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseCharacter
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x



-- Parse expresion
readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ "Parsing: " ++ expr
  putStrLn $ readExpr expr
