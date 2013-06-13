{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module WebApp where

import Snap
import Snap.Snaplet.Heist
import SnapletDatabase hiding (database)
import Control.Lens
import Text.Read
import TypedDatabase hiding (new)
import Text.XmlHtml
import Html
import Data.ByteString.Char8 as B
import Data.Text as T
import qualified Article
import Dialog

data App = App {
  _heist :: Snaplet (Heist App),
  _database :: Snaplet SnapletDatabase,
  _dialog :: Snaplet (Dialog App)}

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

initApp = makeSnaplet "app" "the forward app" Nothing $ do
  hst <- nestSnaplet "heist" heist $ heistInit "templates"
  dtbse <- nestSnaplet "database" database $ initSnapletDatabase
  dialg <- nestSnaplet "dialog" dialog $ dialogInit

  addRoutes [("article/:id", getById (\h -> Article.print h >>= return . (:[]) . TextNode . T.pack)),
             ("new-article", newObject Article.generate)]
  

  return $ App hst dtbse dialg

getById :: Object a => (LVar a -> Property [Node]) -> Handler App App ()
getById f = do
  id <- getParam "id"
  case (id >>= readMaybe . B.unpack) of
    Nothing -> return ()
    Just i -> 
        do
          a <- with database $ SnapletDatabase.getByInt' i
          case a of
            Nothing -> return ()
            Just s -> do
                   t <- with database $ SnapletDatabase.get (f s)
                   renderWithSplices "tmplte" [("contentD", return t)] 
 
newObject :: Object a =>  HtmlGenerate a -> Handler App App ()
newObject gen = method GET $ do
                  lens <- getLens
                  with dialog $ makeDialog gen $ 
                       (>> return ()) . (with database . new)