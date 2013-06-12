{-# LANGUAGE OverloadedStrings, TemplateHaskell, ExistentialQuantification #-}
module Dialog where

import Snap
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString as B
import Database
import Html
import Data.Text.Encoding
import Control.Lens
import Snap.Snaplet.Session.Common
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Data.ByteString.Base64 as Base64
import System.Random.MWC


type DialogManager m = M.Map B.ByteString (DialogData m)

data DialogData m = forall obj. Object obj => DialogData { gen :: Generate HtmlGenerate obj, fun :: obj -> m () }

data Dialog m = Dialog { _session :: MVar (DialogManager m), _rng :: RNG  }

makeLenses ''Dialog

dialogInit = makeSnaplet "dialog" "a dialog applet" Nothing $ do
  addRoutes [("", method POST handleDialog)]
  sess <- liftIO $ newMVar M.empty
  rng <- liftIO mkRNG
  return $ Dialog sess rng 

getRand :: Handler v (Dialog m) T.Text
getRand = do
  (gets _rng >>= liftIO . flip withRNG (\a -> sequence $ take 18 $ repeat $ uniform a) >>= 
   return . decodeUtf8 . Base64.encode . B.pack)

handleDialog :: Handler v (Dialog m) ()
handleDialog = do
  a <- getPostParam "key"
  d <- gets _session
  e <- case a of {Nothing -> pass; Just key -> 
                     liftIO $ modifyMVar d (\m -> 
                                             case M.lookup key m of
                                               Nothing -> return (m, Nothing)
                                               Just r -> return ((M.delete key m), Just r))}  
  maybe (writeBS "No valid request") (const $ writeBS "Succesfully received form") e

makeDialog :: HasHeist v => HtmlGenerate a -> (a -> Handler v r ()) -> Handler v (Dialog m) ()
makeDialog gen fun = do
  url <- liftM (B.append "/") $  snapletURL ""
  prR <- getRand
  withSplices [("form", return $ makeForm (decodeUtf8 url) prR gen)]
    $ render "dialog"