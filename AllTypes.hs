-- internal module : here, all serializations should be saved

module AllTypes where

import Data.Serialize.Get
import Object

getObject :: String -> Get SumObject
getObject = undefined