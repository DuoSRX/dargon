module Lexer where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

type Op a = Ex.Operator String () Identity a
type Operators a = Ex.OperatorTable String () Identity a

reservedNames :: [String]
reservedNames = ["let", "letrec", "in", "fn", "if", "then", "else", "case", "of"]

reservedOpNames :: [String]
reservedOpNames = ["->", "+", "-", "*", "="]

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser Tok.LanguageDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd   = "-}"
  , Tok.commentLine  = "--"
  , Tok.nestedComments = True
  , Tok.caseSensitive = True
  , Tok.reservedNames = reservedNames
  , Tok.reservedOpNames = reservedOpNames
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum Text.Parsec.<|> oneOf "_'"
  , Tok.opStart  = oneOf "-~!#%&*+./<=>\\?@|:^"
  , Tok.opLetter = oneOf "-~!#%&*+./<=>\\?@|:^"
  }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

integer :: Parser Integer
integer = Tok.integer lexer

semi :: Parser String
semi = Tok.semi lexer

comma :: Parser String
comma = Tok.comma lexer

contents :: Parser a -> Parser a
contents c = do
  Tok.whiteSpace lexer
  result <- c
  eof
  return result
