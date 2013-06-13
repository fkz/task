{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module SnapletDatabase where
import Database
import Snap
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import Data.String
import qualified Data.Configurator as C
import qualified Data.Text as T
import Control.Lens
import Data.Tuple

data SnapletDatabase = SnapletDatabase {
  _database :: MVar Database }

makeLenses ''SnapletDatabase

initSnapletDatabase = makeSnaplet "database" "the database backend" Nothing $ do 
                        config <- getSnapletUserConfig
                        st <- liftIO $ C.lookup config "storage" 
                        db <- liftIO $ newMVar $ newDatabase st
                        return $ SnapletDatabase db
   


get :: Property a -> Handler b SnapletDatabase a
get a = do
  db <- liftIO . readMVar =<< use database
  liftIO $ runProperty a db

liftSt :: St a -> Handler b SnapletDatabase a
liftSt q = do
  use database >>= liftIO . flip modifyMVar ((swap <$>) . runSt q)

getByInt :: Object a => Int -> Handler b SnapletDatabase (Maybe (Ref a))
getByInt i = liftSt $ Database.getByInt i
  

new :: Object a => a -> Handler b SnapletDatabase (Ref a)
new = liftSt . Database.new