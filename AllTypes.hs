-- internal module : here, all serializations should be saved

module AllTypes where

import Data.Serialize.Get
import Object
import Article(Article)
import Data.SafeCopy

getObject :: String -> Get SumObject
getObject s = f >>= return . SumObject
  where
    f = case s of
      "article" -> safeGet :: Get Article
      _ -> error ("The getter for " ++ s ++ " is not yet defined")