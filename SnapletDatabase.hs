{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module SnapletDatabase where
import TypedDatabase
import Snap
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import Data.String
import qualified Data.Configurator as C
import qualified Data.Text as T
import Control.Lens
import Data.Tuple

data SnapletDatabase = SnapletDatabase {
  _database :: Database }

makeLenses ''SnapletDatabase

initSnapletDatabase = makeSnaplet "database" "the database backend" Nothing $ do 
                        config <- getSnapletUserConfig
                        st <- liftIO $ C.lookup config "storage" 
                        let loc = maybe "./" id st
                        db <- liftIO $ open loc
                        printInfo (T.pack $ "Using database at " ++ loc)
                        return $ SnapletDatabase db

runDatabase :: DatabaseS a -> Handler b SnapletDatabase a
runDatabase a = do
  db <- use database
  liftIO $ runDatabaseS db a


get :: Property a -> Handler b SnapletDatabase a
get = runDatabase . atomic


--liftSt :: St a -> Handler b SnapletDatabase a
--liftSt q = 
  
  
getByInt :: Object a => Int -> Handler b SnapletDatabase (Maybe (Ref a))
getByInt = runDatabase . getRef

getByInt' :: Object a => Int -> Handler b SnapletDatabase (Maybe (LVar a))
getByInt' i = runDatabase $ do
  getRef i >>= maybe (return Nothing) (\a -> getVar a >>= return . Just) 

new :: Object a => a -> Handler b SnapletDatabase (LVar a)
new = runDatabase . TypedDatabase.new