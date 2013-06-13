{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving,ScopedTypeVariables #-}

module TypedDatabase (
  Object,
  Database,
  DatabaseS,
  Ref,
  LVar,
  Transform,
  readLVar,
  writeLVar,
  atomic,
  getVar,
  varToRef,
  runDatabaseS,
  getRef)
       where

import qualified PersistentDatabase as P
import Data.SafeCopy
import Data.Typeable
import Data.Serialize
import qualified AllTypes
import Object

instance SafeCopy SumObject where
  version = 1
  getCopy = contain $ do
    a <- safeGet :: Get String
    AllTypes.getObject a
  
  putCopy (SumObject m) = contain $ do
    safePut $ name (Proxy m)
    safePut m

toSumObject :: Object a => a -> SumObject
toSumObject = SumObject

fromSumObject :: Object a => SumObject -> Maybe a
fromSumObject (SumObject a) = cast a

newtype Database = Database (P.Database SumObject)
newtype Ref a = Ref (P.Ref SumObject)
newtype LVar a = LVar (P.LVar SumObject)
newtype DatabaseS a = DatabaseS (P.DatabaseS SumObject a) deriving Monad
newtype Transform a = Transform (P.Transform SumObject a) deriving Monad

readLVar :: Object a => LVar a -> Transform a
readLVar (LVar a) = Transform $ P.readLVar a >>= return . just . fromSumObject
  where just (Just a) = a

writeLVar :: Object a => LVar a -> a -> Transform () 
writeLVar (LVar a) b = Transform $ P.writeLVar a (toSumObject b)

atomic :: Transform a -> DatabaseS a
atomic (Transform a) = DatabaseS (P.atomic a)

getVar :: Ref a -> DatabaseS (LVar a)
getVar (Ref a) = DatabaseS (P.loadRef a >>= return . LVar)

varToRef :: LVar a -> Ref a
varToRef (LVar a) = Ref $ P.varToRef a

runDatabaseS :: Database -> DatabaseS a -> IO a
runDatabaseS (Database d) (DatabaseS s) = P.runDatabaseS d s

getRef :: Object a => Int -> DatabaseS (Maybe (Ref a))
getRef i = DatabaseS $ do
  obj <- P.getRef i
  case obj of
    Nothing -> return Nothing
    Just ref -> P.loadRef ref >>= \var -> P.atomic $ do
      refV <- P.readLVar var
      let comb :: Maybe a -> Maybe (Ref a)
          comb s = s >> Just (Ref (P.varToRef var)) in
        return (comb (fromSumObject refV))