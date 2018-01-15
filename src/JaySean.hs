module JaySean where

import           Data.List (intercalate)

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
