module JaySean where

import           Data.List (intercalate, intersperse)

newtype KV =
  KV (String, Json)

data Json
  = JString String
  | JNumber Double
  | JObject [KV]
  | JArray [Json]
  | JBool Bool
  | JNull

instance Show Json where
  show (JString str) = show str
  show (JNumber num) = show num
  show (JObject kvs) = "{" ++ lst ++ "}"
    where
      lst = intercalate " , " $ map show kvs
  show (JArray arr) = show arr
  show (JBool True) = "true"
  show (JBool False) = "false"
  show JNull = "null"

instance Show KV where
  show (KV (key, json)) = show key ++ " : " ++ show json

json =
  JArray
    [ JObject [KV ("Key1", JObject [KV ("Key 3", JString "Value1"), KV ("Key 4", JNull)])]
    , JObject [KV ("Key2", JNull)]
    , JObject [KV ("Key5", JNull)]
    , JArray []
    , JArray
        [ JNumber 3
        , JBool False
        , JObject
            [ KV
                ( "Hi"
                , JArray
                    [ JString "yoyo"
                    , JString "honey"
                    , JObject [KV ("BookingId", JObject [KV ("BookingId", JString "Value1"), KV ("Key 4", JNull)])]
                    ])
            ]
        ]
    ]
