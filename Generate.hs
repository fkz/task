module Generate where

import Database
import Control.Applicative

class Applicative m => BasicGenerator m where
  message :: String -> m ()
  -- |getByNumber takes a name and outputs an input box with previous text in it and  with that name a string
  getByName :: String -> String -> m String
  --getNumber :: String -> m Integer
  mistake :: String -> m a
  
  --getNumber s = maybeRead <$> getByName s ""