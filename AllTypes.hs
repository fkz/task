-- internal module : here, all serializations should be saved

module AllTypes where

import Data.Serialize.Get
import Object
--import Article(Article)

getObject :: String -> Get SumObject
getObject = undefined
--getObject s = if s == name (Proxy (undefined :: Article)) then safeGet >>= SumObject