module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (many1)

import Text.Parsec.Expr as Ex

import Lexer
import Types

number :: Parser Exp
number = ELit . LInt <$> integer

variable :: Parser Exp
variable = EVar <$> identifier

bool :: Parser Exp
bool = true <|> false
  where true  = reserved "True" >> return (ELit (LBool True))
        false = reserved "False" >> return (ELit (LBool False))

charLit :: Parser Exp
charLit = do
  char '\''
  c <- letter
  char '\''
  return $ ELit (LChar c)

stringLit :: Parser Exp
stringLit = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ ELit (LString str)

expr :: Parser Exp
expr = foldl1 EApp <$> many1 term

allExp :: Parser Exp
allExp =     parens expr
         <|> number
         <|> stringLit
         <|> charLit
         <|> bool
         <|> letform
         <|> ifThenElse
         <|> lambda
         <|> variable

letform :: Parser Exp
letform = do
  reserved "let"
  ident <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet ident e1 e2

lambda :: Parser Exp
lambda = do
  reserved "fn"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr ELam body args

ifThenElse :: Parser Exp
ifThenElse = do
  reserved "if"
  pred <- expr
  reserved "then"
  t <- expr
  reserved "else"
  e <- expr
  return $ EIf pred t e

-- Represents a top level declaration, binding a name to an expression.
type Binding = (String, Exp)

topLevelLet :: Parser Binding
topLevelLet = do
  reserved "let"
  ident <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  semi
  return (ident, foldr ELam body args)

-- FIXME: Duplication
topLevelLetrec :: Parser Binding
topLevelLetrec = do
  reserved "letrec"
  ident <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  semi
  return (ident, EFix $ foldr ELam body (ident:args))

-- Useful for the REPL
topLevelValue :: Parser Binding
topLevelValue = do
  expression <- expr
  return ("expr", expression)

topLevel :: Parser Binding
topLevel = try topLevelLet <|> topLevelLetrec <|> topLevelValue

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

-- TODO: Add eql, mul..
operatorsTable :: Operators Exp
operatorsTable = [
  [
    infixOp "*" (EOp Mul) Ex.AssocLeft
  ],
  [
    infixOp "+" (EOp Add) Ex.AssocLeft
  , infixOp "-" (EOp Sub) Ex.AssocLeft
  ],
  [
    infixOp "==" (EOp Eql) Ex.AssocLeft
  ]
  ]

term :: Parser Exp
term = Ex.buildExpressionParser operatorsTable allExp

parseExp :: String -> Either ParseError Exp
parseExp = parse (contents expr) ""

parseProgram :: String -> Either ParseError [Binding]
parseProgram = parse (contents (many topLevel)) ""
