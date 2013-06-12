{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances #-}

module Database (
  Ref,
  St,
  runSt,
  getByInt,
  new,
  get,
--  (.=),
  (.^),
  Property,
  Object,
  runProperty,
  Database,
  newDatabase,
  deriveObject,
  liftG,
  liftP,
  Generate)
where
  
import qualified Language.Haskell.TH as TH  
import qualified Data.Sequence as S
import Data.SafeCopy
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Monad.State.Strict as SM
import Data.Typeable
import Data.Maybe
import Control.Lens hiding ((.=))
import Control.Applicative
import Control.Concurrent.STM

data Id = Id {identifier :: Int, pointer' :: Maybe (TVar SumObject)}

pointer :: Database -> Id -> TVar SumObject
pointer d i = case pointer' i of
  Just x -> x
  Nothing -> S.index (runDatabase d) (identifier i)
-- the convention is, that there will be only added elements at the end of the sequence 
data Database = Database {runDatabase :: S.Seq (TVar SumObject) }

newDatabase :: Maybe FilePath -> Database
newDatabase _ = Database S.empty

insertDatabase :: Object a => a -> Database -> IO (Id, Database)
insertDatabase a d = do
  obj <- atomically $ newTVar (SumObject a)
  let result = Database $ (runDatabase d) S.|> obj in
    return (Id (S.length (runDatabase d)) (Just obj), result)

getDatabase :: Object a => Database -> Id -> STM a
--getDatabase d (Id i) = readTVar (S.index (runDatabase d) i) >>= return . fromJust . fromSumObject
getDatabase d i = readTVar (pointer d i) >>= return . fromJust . fromSumObject

updateDatabase :: Object a => Id -> a -> Database -> STM Database
--updateDatabase (Id i) a d =  Database $ S.update i (toSumObject a) (runDatabase d)
updateDatabase i a r = writeTVar (pointer r i) (toSumObject a) >> return r


data Proxy a = Proxy
class (SafeCopy a, Typeable a) => Object a where
  name :: Proxy a -> String

deriveObject :: Version a -> String -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
deriveObject a s b c = return concat `ap` sequence [deriveSafeCopy a b c, makeLenses c, 
                     [d| instance Object $(TH.conT c) where name _ = $(TH.litE (TH.stringL s)) |]] 

data SumObject = forall a. Object a => SumObject a

toSumObject :: Object a => a -> SumObject
toSumObject = SumObject

fromSumObject :: Object a => SumObject -> Maybe a
fromSumObject (SumObject a) = cast a

newtype St a = St (StateT Database IO a) deriving Monad
newtype Ref a = Ref {runRef :: Id}

instance SafeCopy Id where
  version = 1
  getCopy = contain (safeGet >>= return . flip Id Nothing) 
  putCopy (Id a _) = contain (safePut a)

--deriveSafeCopy 1 'base ''Id
deriveSafeCopy 1 'base ''Ref

new :: Object a => a -> St (Ref a)
new a = St $ do
  d <- SM.get
  (i,d') <- liftIO $ insertDatabase a d
  SM.put d'
  return (Ref i)

(.=) :: Object a => a -> Ref a ->  St ()
a .= (Ref r) = St $ do
  d <- SM.get
  s <- liftIO $ atomically (updateDatabase r a d)
  SM.put s

class Monad m => ReadObjects m where
  get :: Object a => Ref a -> m a
  
instance ReadObjects St where
  get (Ref a) = St $ SM.gets (flip getDatabase a) >>= liftIO . atomically

(.^) :: (ReadObjects m, Object s) => Ref s -> Getting a s a -> m a
s .^ a = do
  s' <- get s
  return (s'^.a)

newtype Property a = Property (St a) deriving (Monad, ReadObjects)

newtype Generate m a = Generate {runGenerate :: Property (m a)}

instance Functor m => Functor (Generate m) where
  fmap f (Generate x) = Generate $ ap (return (fmap f)) x

instance Applicative m => Applicative (Generate m) where
  pure = Generate . return . pure
  (Generate f) <*> (Generate x) = Generate $ liftM2 (<*>) f x 

liftG :: m a -> Generate m a
liftG = Generate . return

liftP :: Applicative m => Property a -> Generate m a
liftP a = Generate (a >>= return . pure)

runProperty :: Property a -> Database -> IO a
runProperty (Property st) d = runSt st d >>= return . fst

runSt :: St a -> Database -> IO (a, Database)
runSt (St a) d = runStateT a d

getByInt :: Object a => Int -> St (Maybe (Ref a))
getByInt i = St $ do
  db <- SM.gets runDatabase
  if i < 0 || i > S.length db then
    return Nothing
  else do
    let obj = S.index db i
    inside <- liftIO $ atomically $ readTVar obj
    case fromSumObject inside of
      Nothing -> return Nothing
      Just t -> return (Just (ref t (Id i (Just obj)))) 
    where
     ref :: a -> Id -> Ref a
     ref _ = Ref