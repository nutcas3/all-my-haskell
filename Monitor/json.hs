module System.Remote.Json
    (
      encodeAll
    , encodeOne
    ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L

import Monitor
import qualified Monitor.Json as Json

-- | Encode metrics as nested JSON objects. See 'Json.sampleToJson'
-- for a description of the encoding.
encodeAll :: Sample -> L.ByteString
encodeAll = A.encode . Json.sampleToJson

-- | Encode metric a JSON object. See 'Json.valueToJson'
-- for a description of the encoding.
encodeOne :: Value -> L.ByteString
encodeOne = A.encode . Json.valueToJson