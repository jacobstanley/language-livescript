module Language.Livescript.Parser.Type (
      Parser
    , ParserState
    , Operator
    ) where

import           Control.Monad.State
import           Data.Text (Text)
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P
import           Text.Parsec.Text ()

------------------------------------------------------------------------

type Parser a = P.ParsecT Text () ParserState a

type ParserState = State P.SourcePos

type Operator a = P.Operator Text () ParserState a
