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


json :: Parser JSON
json = ws *> jsValue

jsValue :: Parser JSON
jsValue = choice [jsNull,jsBool,jsObject,jsArray,jsNumber,jsString

-- |
--
-- >>> parseJSON "  null "
-- Right JSNull
jsNull :: Parser JSON
jsNull = jsAtom "null" JSNull

-- |
--
-- >>> parseJSON "  false "
-- Right (JSBool False)
-- >>> parseJSON "true"
-- Right (JSBool True)
jsBool :: Parser JSON
jsBool = jsAtom "false" (JSBool False)
     <|> jsAtom "true"  (JSBool True)

-- |
--
-- >>> parseJSON "  { \"key1\": 2 ,  \"key2\" : false } "
-- Right (JSObject [("key1",JSNumber 2),("key2",JSBool False)])
jsObject :: Parser JSON
jsObject = JSObject <$> betweenWs '{' kvs '}'
  where
    kvs = kv `sepBy` charWs ','
    kv = do
        JSString key <- jsString
        charWs ':'
        val <- jsValue
        return (key, val)