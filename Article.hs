{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Article where

import TypedDatabase
import Data.Typeable
import Data.SafeCopy
import Control.Lens
import Control.Monad
import Generate
import Control.Applicative
import Control.Lens
import Object
import DeriveObject

data Article = Article {
  _title:: String,
  _content:: String, 
  _next :: Maybe (Ref Article) } deriving Typeable

deriveObject' 1 "article" 'base ''Article

print :: LVar Article -> Property String
print a = do
  av <- readLVar a
  return $ av^.title ++ av^.content

generate :: BasicGenerator m => m Article
generate =  Article <$ message "Bitte gebe einen Artikel ein" <*>
                              getByName "Titel: " "" <*>
                              getByName "Inhalt: " "" <*>
                              pure Nothing