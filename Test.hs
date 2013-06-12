{-# LANGUAGE Rank2Types, DeriveDataTypeable, ExistentialQuantification #-}

import Data.Typeable
import Control.Exception

class Typeable a => BigSum a where
  from :: Welt -> Maybe a
  to :: a -> Welt
  
  from (Welt a) = cast a
  to = Welt

data Welt = forall a. BigSum a => Welt a deriving (Typeable)

data D1 = D1 String deriving (Typeable,Show)

instance BigSum D1

d1 = D1 "Hallo"

dw = Welt d1