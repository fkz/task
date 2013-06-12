{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module SnapletDatabase where
import Database
import Snap
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Data.String
import qualified Data.Configurator as C
import qualified Data.Text as T
import Control.Lens

data SnapletDatabase = SnapletDatabase {
  _database :: TVar Database }

makeLenses ''SnapletDatabase

initSnapletDatabase = makeSnaplet "database" "the database backend" Nothing $ do 
                        config <- getSnapletUserConfig
                        st <- liftIO $ C.lookup config "storage" 
                        db <- liftIO $ newTVarIO $ newDatabase st
                        return $ SnapletDatabase db

get :: Property a -> Handler b SnapletDatabase a
get a = do
  dbpr <- use database
  db <- liftIO $ atomically $ readTVar dbpr 
  liftIO $ runProperty a db

liftSt :: St a -> Handler b SnapletDatabase a
liftSt q = do
  dbs_ptr <- use database
  dbs <- liftIO $ atomically $ readTVar dbs_ptr 
  (a,dbs') <- liftIO $ runSt q dbs
  liftIO $ atomically $ writeTVar dbs_ptr dbs'
  return a

getByInt :: Object a => Int -> Handler b SnapletDatabase (Maybe (Ref a))
getByInt i = liftSt $ Database.getByInt i
  