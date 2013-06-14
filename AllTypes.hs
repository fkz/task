-- internal module : here, all serializations should be saved
{-# LANGUAGE GADTs, TemplateHaskell #-}

module AllTypes where

import Data.Serialize.Get
import Object
import Article(Article, Category)
import Data.SafeCopy

getObject :: String -> Get SumObject
getObject s = f
  where
    t s = s >>= return . SumObject
    f = case s of
      "article" -> t (safeGet :: Get Article)
      "category" -> t (safeGet :: Get Category)
      _ -> error ("The getter for " ++ s ++ " is not yet defined")

--data Property a where
--  ConstProperty :: a -> Property a
--  IntProperty :: Property Int
  
--deriveSafeCopy 1 'base ''Property