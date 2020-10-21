-- | Write Yourself a Scheme

{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import System.Environment
import Numeric (readHex, readOct)

-- Acceptable terminal symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- Make the parser ignore whitespaces
spaces :: Parser ()
spaces = skipMany1 space


-- Basic strucutres of our implementation of Scheme
data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | Character Char
                | Float Float
               
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
type ThrowsError = Either LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
-- Error handling
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


-- Primitives --
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", typecheckOp boolcheckComp),
              ("symbol?",  typecheckOp symbcheckComp),
              ("string?",  typecheckOp strcheckComp),
              ("number?",  typecheckOp numcheckComp),
              ("symbol->string", symbToString),
              ("string->symbol", stringToSymb),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", isEqv),
              ("equal?", equal),
              ("eqv?", isEqv)]

-- TODO: add arithemtic functionality for floating point numbers
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _           []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params@(Number _:_)        = mapM unpackNum params >>= return . Number . foldl1 op
-- FIXME: numericBinop op params@(Float _:_)        = mapM unpackFloat params >>= return . Float . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "Number" notNum

-- FIXME:
unpackFloat :: LispVal -> ThrowsError Float
unpackFloat (Float n) = return n
unpackFloat (String n) = let parsed = reads n in
                           if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ head parsed
unpackFloat (List [n]) = unpackFloat n
unpackFloat notNum     = throwError $ TypeMismatch "Float" notNum


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                      left <- unpacker $ (head args)
                                      right <- unpacker $ args !! 1
                                      return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
floatBoolBinop  = boolBinop unpackFloat
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Float s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

symbToString :: [LispVal] -> ThrowsError LispVal
symbToString [Atom s] = return $ String s
symbToString a@(Atom _:_) = throwError $  NumArgs 1 a
symbToString x = throwError $ TypeMismatch "Atom" $ head x

stringToSymb :: [LispVal] -> ThrowsError LispVal
stringToSymb [String s] = return $ Atom s
stringToSymb a@(String _:_) = throwError $  NumArgs 1 a
stringToSymb x = throwError $ TypeMismatch "String" $ head x

isEqv :: [LispVal] -> ThrowsError LispVal
isEqv ((Atom x):(Atom y):_) = return $ Bool $ x == y
isEqv ((Number x):(Number y):_) = return $ Bool $ x == y
isEqv ((Float x):(Float y):_) = return $ Bool $ x == y
isEqv ((String x):(String y):_) = return $ Bool $ x == y
isEqv ((Character x):(Character y):_) = return $ Bool $ x == y
isEqv x = throwError $ TypeMismatch "comparison of different types" $ head x

typecheckOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
typecheckOp op params = if length params > 1 then throwError $ NumArgs 1 params else return $ Bool $ op $ head params

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList  = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1,x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

unPackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unPackEquals arg1 arg2 (AnyUnpacker unpacker) =
                do unpacked1 <- unpacker arg1
                   unpacked2 <- unpacker arg2
                   return $ unpacked1 == unpacked2
                `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
                        pEq <-  liftM or $ mapM (unPackEquals arg1 arg2)
                                [AnyUnpacker unpackNum, AnyUnpacker unpackFloat, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                        eqvEquals <- isEqv [arg1, arg2]
                        return $ Bool $ (pEq || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
-- Evaluation --
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Atom _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Character _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of
             Bool False -> eval alt
             Bool True -> eval conseq
             _  -> throwError $ TypeMismatch "Not boolean" pred
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Main Function --
main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled
--- main = getArgs >>= print . eval . readExpr . head
{-
main = do
  (expr:_) <- getArgs
  putStrLn $ "Parsing: " ++ expr
  putStrLn  (show  (readExpr  expr))
-}
