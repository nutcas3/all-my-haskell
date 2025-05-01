-- | This JSON package retains the order of array elements.
--   JSON: http://www.ietf.org/rfc/rfc4627.txt

module JSON (
    JSON(..)
  , parseJSON
  ) where

import Control.Applicative ((<*),(*>),(<$>),(<$))
import Control.Monad (void)
import Data.Char
import Data.List (foldl')
import Text.Parsec
import Text.Parsec.String


parseJSON :: String -> Either ParseError JSON
parseJSON xs = parse json "json" xs

data JSON = JSNull
          | JSBool Bool
          | JSNumber Int
          | JSString String
          | JSArray [JSON]
          | JSObject [(String,JSON)]
          deriving (Show, Eq)