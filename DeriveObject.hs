{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module DeriveObject (deriveObject') where

import Object
import qualified Language.Haskell.TH as TH  
import Control.Lens
import Data.SafeCopy
import Control.Monad


deriveObject' :: Version a -> String -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
deriveObject' a s b c = return concat `ap` sequence [deriveSafeCopy a b c, makeLenses c, 
                     [d| instance Object $(TH.conT c) where name _ = $(TH.litE (TH.stringL s)) |]] 
