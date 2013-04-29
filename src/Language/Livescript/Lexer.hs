module Language.Livescript.Lexer (
      lexeme
    , identifier
    , reserved
    , operator
    , reservedOp
    , charLiteral
    , stringLiteral
    , natural
    , integer
    , float
    , naturalOrFloat
    , decimal
    , hexadecimal
    , octal
    , symbol
    , whiteSpace
    , parens
    , braces
    , brackets
    , squares
    , semi
    , comma
    , colon
    , dot
    , identifierStart
    ) where

import           Data.Text (Text)
import           Prelude hiding (lex)
import           Text.Parsec hiding (State)
import           Text.Parsec.Text ()
import qualified Text.Parsec.Token as T

import           Language.Livescript.Parser.Type

------------------------------------------------------------------------

identifierStart :: Parser Char
identifierStart = letter <|> oneOf "$_"

livescriptDef :: T.GenLanguageDef Text () ParserState
livescriptDef = T.LanguageDef {
      T.caseSensitive   = True
    , T.commentStart    = "/*"
    , T.commentEnd      = "*/"
    , T.commentLine     = "#"
    , T.nestedComments  = False
    , T.identStart      = identifierStart
    , T.identLetter     = alphaNum <|> oneOf "$_-"
    , T.opStart         = oneOf "=:+-"
    , T.opLetter        = oneOf "="
    , T.reservedNames   = names
    , T.reservedOpNames = ops
    }
  where
    names = []
    ops   = ["=",":=","+","-"]

lex :: T.GenTokenParser Text () ParserState
lex = T.makeTokenParser livescriptDef

-- everything but commaSep and semiSep
identifier :: Parser String
identifier = T.identifier lex

reserved :: String -> Parser ()
reserved = T.reserved lex

operator :: Parser String
operator = T.operator lex

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lex

charLiteral :: Parser Char
charLiteral = T.charLiteral lex

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lex

natural :: Parser Integer
natural = T.natural lex

integer :: Parser Integer
integer = T.integer lex

float :: Parser Double
float = T.float lex

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = T.naturalOrFloat lex

decimal :: Parser Integer
decimal = T.decimal lex

hexadecimal :: Parser Integer
hexadecimal = T.hexadecimal lex

octal :: Parser Integer
octal = T.octal lex

symbol :: String -> Parser String
symbol = T.symbol lex

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lex

parens :: Parser a -> Parser a
parens = T.parens lex

braces :: Parser a -> Parser a
braces = T.braces lex

squares :: Parser a -> Parser a
squares = T.squares lex

semi :: Parser String
semi = T.semi lex

comma :: Parser String
comma = T.comma lex

colon :: Parser String
colon = T.colon lex

dot :: Parser String
dot = T.dot lex

brackets :: Parser a -> Parser a
brackets = T.brackets lex

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lex
