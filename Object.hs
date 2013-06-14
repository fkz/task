{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}

-- this module should not be imported from normal code, instead import 
-- TypedDatabase which exports Object which is normally all you need

module Object where
import Data.SafeCopy
import Data.Typeable
--import Properties

data Proxy a = Proxy a
class (SafeCopy a, Typeable a) => Object a where
  name :: Proxy a -> String
--  toProperty :: Typeable b => a -> Maybe SomeProperty

--  toProperty = const Nothing

data SumObject = forall a. Object a => SumObject a

