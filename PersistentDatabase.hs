{-# LANGUAGE TupleSections #-}

module PersistentDatabase (
  Ref,
  LVar,
  readLVar,
  writeLVar,
  DatabaseS,
  Database,
  Transform,
  open,
  getRef,
  loadRef,  
  new,
  runDatabaseS
  ) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.IO.Error
import qualified Data.ByteString as B
import Data.SafeCopy
import Data.Serialize
import qualified Data.List as L

-- a database which saves objects of type b
data Database b = Database {loadedData :: TVar (M.Map Int (Maybe (TVar b))), path :: FilePath, maxV :: TVar Int, writeOut :: TChan (LVar b) }

data Ref b = Ref { ident :: Int, value :: Maybe (TVar b) }

type DatabaseS b = ReaderT (Database b) IO

data LVar b = LVar { identL :: Int, valueL :: TVar b }

readLVar :: LVar b -> Transform b b
readLVar l@(LVar _ t) = Transform $ do  
  var <- readTVar t
  return (var, [])

writeLVar :: LVar b -> b -> Transform b ()
writeLVar l@(LVar _ t) b = Transform $ do
  writeTVar t b
  return ((),[l])

newtype Transform b a = Transform {runTransform :: (STM (a,[LVar b]))}

instance Monad (Transform b) where
  return = Transform . return . (,[]) 
  d >>= g = Transform $ do
      (a,vars) <- runTransform d
      (b,vars2) <- runTransform (g a)
      return (b,vars ++ vars2)
      
getFilename :: Ref b -> DatabaseS b FilePath
getFilename b = do
  fn <- asks path
  return (fn ++ show (ident b))

open :: SafeCopy b => FilePath -> IO (Database b)
open fp = do
  m <- newTVarIO M.empty
  s <- catchIOError (readFile (fp ++ "max")) (\e -> if isDoesNotExistError e then return "0" else ioError e)
  s' <- newTVarIO (read s)
  tc <- newTChanIO
  let result = Database m fp s' tc
  forkIO (runDatabaseS result alwaysFlush)
  return result

getRef :: Int -> DatabaseS b (Maybe (Ref b))
getRef i = do
  (asks maxV >>= \b -> liftIO $ atomically $ readTVar b >>= return . ( > i))
    >>= \a -> return $ if a then Just (Ref i Nothing) else Nothing

loadFromFile :: SafeCopy b => Ref b -> DatabaseS b b
loadFromFile f = do
  filename <- getFilename f
  contents <- liftIO $ B.readFile filename
  case runGet safeGet contents of 
    Left s -> fail s
    Right r -> return r

writeToFile :: SafeCopy b => Ref b -> b -> DatabaseS b ()
writeToFile f b = do
  filename <- getFilename f
  liftIO $ B.writeFile filename $ runPut (safePut b)

loadRef :: SafeCopy b => (Ref b) -> DatabaseS b (LVar b)
loadRef l@(Ref a (Just b)) = return $ LVar a b
loadRef l@(Ref i Nothing) = do
  dtbse <- asks loadedData
  b <- liftIO . atomically $ do
    m <- readTVar dtbse
    case M.lookup i m of
      Just Nothing -> retry
      Just (Just a) -> return $ Just a
      Nothing -> return Nothing
  case b of
    Nothing -> do
      r <- loadFromFile l
      rIn <- liftIO $ newTVarIO r
      liftIO . atomically $
        readTVar dtbse >>= writeTVar dtbse . M.insert i (Just rIn)
      return (LVar i rIn)
    Just a -> 
      return (LVar i a)

new :: b -> DatabaseS b (LVar b)
new b = do
  maxR <- asks maxV      
  ident <- liftIO $ atomically $ do
    m <- readTVar maxR
    writeTVar maxR (m+1)
    return m
  dtbse <- asks loadedData
  chan <- asks writeOut
  liftIO $ atomically $ do
    dtb <- readTVar dtbse
    b' <- newTVar b
    let lv = LVar ident b'
    writeTChan chan lv
    writeTVar dtbse (M.insert ident (Just b') dtb)
    return lv
  
-- flush the next item (might block until the next item becomes available)
flush :: SafeCopy b => DatabaseS b ()
flush = do
  chan <- asks writeOut
  (ident, value) <- liftIO $ atomically $ do
    v <- readTChan chan
    value <- readTVar (valueL v)
    return (varToRef v, value)
  writeToFile ident value

alwaysFlush :: SafeCopy b => DatabaseS b ()
alwaysFlush = PersistentDatabase.flush >> alwaysFlush

varToRef :: LVar b -> Ref b
varToRef (LVar a b) = Ref a (Just b)

runDatabaseS :: Database b -> DatabaseS b s -> IO s
runDatabaseS = flip runReaderT
