{-# LANGUAGE ExistentialQuantification #-}

-- this module should not be imported from normal code, instead import 
-- TypedDatabase which exports Object which is normally all you need

module Object where
import Data.SafeCopy
import Data.Typeable


data Proxy a = Proxy a
class (SafeCopy a, Typeable a) => Object a where
  name :: Proxy a -> String


data SumObject = forall a. Object a => SumObject a
