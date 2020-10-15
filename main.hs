-- | Write Yourself a Scheme

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Numeric (readHex, readOct)

-- Acceptable terminal symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
                | Character Char
                | Float Float

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
                 string "#\\"
                 x <- parseCharName <|> anyChar
                 return $ Character x
               
parseCharName :: Parser Char
parseCharName = do
                x <- try (string "space" <|> string "newline")
                return $ case x of
                        "space" -> ' '
                        "newline" -> '\n'

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
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ Atom atom

intOrFloat :: String -> LispVal
intOrFloat s = if '.' `elem` s then Float (read s) else Number (read s)

-- Taken from https://tehgeekmeister.wordpress.com/2008/01/11/one-line-binary-reader-in-haskell/
readBin :: String -> Integer
readBin = (`div` 2) . foldl (\x y -> (x + y) * 2) 0 . map (\c -> case c of {'0' -> 0; '1' -> 1; _ -> error "Input is not a binary string."})

parseBool :: Parser LispVal
parseBool = do
                char '#'
                x <- oneOf "tf"
                return $ case x of
                                't' -> Bool True
                                'f' -> Bool False

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
                f <- oneOf "#" <|> digit
                x <- many (alphaNum <|> oneOf ".bodx")
                return (case f of
                                '#' -> case head x of
                                                'b' -> Number $ readBin  $ tail x
                                                'o' -> Number $ fst $ head (readOct $ tail x)
                                                'd' -> intOrFloat $ tail x
                                                'x' -> Number $ fst $ head (readHex $ tail x )
                                _ -> intOrFloat (f:x))

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
parseExpr = parseString
         <|> parseAtom
         <|> try parseCharacter
         <|> try parseBool
         <|> try parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x



-- Parse expresion
readExpr :: String -> LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

-- Displaying Values --
unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "'" ++ [contents] ++ "'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List content) = "(" ++ (unwordList content) ++ ")"
showVal (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail  ++ ")"

instance Show LispVal where show = showVal

-- Primitives --
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol->string", symbToString),
              ("string->symbol", stringToSymb),
              ("boolean?", typecheckOp boolcheckComp),
              ("symbol?",  typecheckOp symbcheckComp),
              ("string?",  typecheckOp strcheckComp),
              ("number?",  typecheckOp numcheckComp)]
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
{-
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
-}
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

symbToString :: [LispVal] -> LispVal
symbToString (Atom s:_) = String s
symbToString _ = String ""

stringToSymb :: [LispVal] -> LispVal
stringToSymb (String s:_) = Atom s
stringToSymb _  = String ""

typecheckOp :: (LispVal -> Bool) -> [LispVal] -> LispVal
typecheckOp op params = Bool $ op $ head params

boolcheckComp :: LispVal -> Bool
boolcheckComp (Bool _) = True
boolcheckComp _ = False

symbcheckComp :: LispVal -> Bool
symbcheckComp (Atom _) = True
symbcheckComp _ = False

numcheckComp :: LispVal -> Bool
numcheckComp (Number _) = True
numcheckComp _ = False

strcheckComp :: LispVal -> Bool
strcheckComp (String _) = True
strcheckComp _ = False


-- Evaluation --
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Atom _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- Main Function --
main :: IO ()
main = getArgs >>= print . eval . readExpr . head
{-
main = do
  (expr:_) <- getArgs
  putStrLn $ "Parsing: " ++ expr
  putStrLn  (show  (readExpr  expr))
-}
