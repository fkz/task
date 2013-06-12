{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Article where

import Database
import Data.Typeable
import Data.SafeCopy
import Control.Lens
import Control.Monad
import Generate
import Control.Applicative

data Article = Article {
  _title:: String,
  _content:: String, 
  _next :: Maybe (Ref Article) } deriving Typeable

deriveObject 1 "article" 'base ''Article

print :: Ref Article -> Property String
print a = do
   return (++) `ap` (a.^title) `ap` (a.^content)


generate :: BasicGenerator m => m Article
generate =  Article <$ message "Bitte gebe einen Artikel ein" <*>
                              getByName "Titel: " "" <*>
                              getByName "Inhalt: " "" <*>
                              pure Nothing