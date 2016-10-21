
module Parser where

import           Text.Parsec
import           Text.Parsec.String
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language   (haskellDef)


lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer = P.makeTokenParser haskellDef

natural :: Parser Integer
natural = P.natural lexer
symbol :: String -> Parser String
symbol = P.symbol lexer
comma :: Parser String
comma  = P.comma lexer

