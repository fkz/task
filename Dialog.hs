{-# LANGUAGE OverloadedStrings, TemplateHaskell, ExistentialQuantification #-}
module Dialog where

import Snap
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString as B
import TypedDatabase
import Html
import Data.Text.Encoding
import Control.Lens
import Snap.Snaplet.Session.Common
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Data.ByteString.Base64 as Base64
import System.Random.MWC


type DialogManager m = M.Map B.ByteString (DialogData m)

data DialogData m = forall obj. Object obj => DialogData { gen :: HtmlGenerate obj, fun :: obj -> m () }

data Dialog m = Dialog { _session :: MVar (DialogManager (Handler  m m)), _rng :: RNG  }

makeLenses ''Dialog

dialogInit = makeSnaplet "dialog" "a dialog applet" Nothing $ do
  addRoutes [("", method POST handleDialog)]
  sess <- liftIO $ newMVar M.empty
  rng <- liftIO mkRNG
  return $ Dialog sess rng 

getRand :: Handler m (Dialog m) T.Text
getRand = do
  (gets _rng >>= liftIO . flip withRNG (\a -> sequence $ take 18 $ repeat $ uniform a) >>= 
   return . decodeUtf8 . Base64.encode . B.pack)

handleDialog :: Handler m (Dialog m) ()
handleDialog = do
  a <- getPostParam "key"
  d <- gets _session
  e <- case a of {Nothing -> pass; Just key -> 
                     liftIO $ modifyMVar d (\m -> 
                                             case M.lookup key m of
                                               Nothing -> return (m, Nothing)
                                               Just r -> return ((M.delete key m), Just r))}  
  case e of
    Nothing -> writeBS "no valid request (token not found)"
    Just (DialogData o w) -> getRequest >>= \request -> 
      case htmlCalculate o request of
           Nothing -> writeBS "you made a mistake during the submitting, sorry"
           Just m -> getRequest >>= \request -> withTop' id (w m)
    
    
makeDialog :: (HasHeist m, Object a) => HtmlGenerate a -> (a -> Handler m m ()) -> Handler m (Dialog m) ()
makeDialog gen fun = do
  url <- liftM (B.append "/") $  snapletURL ""
  prR <- getRand
  
  
  
  use session >>= liftIO . flip modifyMVar_ (return . M.insert (encodeUtf8 prR) (DialogData gen fun))  

  withSplices [("form", return $ makeForm (decodeUtf8 url) prR gen)]
    $ render "dialog"