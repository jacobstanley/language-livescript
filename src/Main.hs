module Main (main) where

import           Control.Applicative hiding (many)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.Environment (getArgs)
import           Text.Parsec hiding ((<|>), parse, State)
import           Text.Parsec.Expr hiding (Operator)
import           Text.Parsec.Indent
import           Text.Parsec.Text ()
import           Text.Show.Pretty (ppShow)

import           Language.Livescript.Lexer
import           Language.Livescript.Parser.Type

main :: IO ()
main = do
    args <- getArgs
    mapM_ parseAndCheck args

parseAndCheck :: FilePath -> IO ()
parseAndCheck path = do
    putStrLn path
    txt <- T.readFile path
    case parse blockP path txt of
        Left err -> print err
        Right x  -> putStrLn (ppShow x)

------------------------------------------------------------------------

parse :: Parser a -> SourceName -> Text -> Either ParseError a
parse p source input = runIndent source (runParserT p () source input)

------------------------------------------------------------------------

data Expr
    = Block [Expr]
    | Var String
    | Literal String

    -- assignment
    | Assign Expr Expr
    | ReAssign Expr Expr

    -- binary operators
    | Add Expr Expr
    | Sub Expr Expr
    deriving (Show,Eq,Ord)

------------------------------------------------------------------------

blockP :: Parser Expr
blockP = Block <$> exprP `sepEndBy` newlineP

exprP :: Parser Expr
exprP = buildExpressionParser operators (lexeme termP)

termP :: Parser Expr
termP = varP <|> literalP

varP :: Parser Expr
varP = Var <$> identifier

literalP :: Parser Expr
literalP = Literal . show <$> decimal

operators :: [[Operator Expr]]
operators =
  [ [ assocLeft "+" Add
    , assocLeft "-" Sub
    ]
  , [ assocLeft "="  Assign
    , assocLeft ":=" ReAssign
    ]
  ]

assocLeft :: String -> (a -> a -> a) -> Operator a
assocLeft op f = Infix (reservedOp op *> pure f) AssocLeft

newlineP :: Parser ()
newlineP = lexeme (oneOf ";\r\n") *> return ()
