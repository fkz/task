{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances #-}
module TestPersistentDatabase where
import PersistentDatabase
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO.Temp
import Data.SafeCopy

addValues :: [b] -> DatabaseS b [(LVar b, b)]
addValues list = sequence $ map (\v -> new v >>= return . (,v)) list

newSampleDatabase :: Arbitrary b => Gen (DatabaseS b [(LVar b,b)])
newSampleDatabase = do
  list <- arbitrary
  return $ addValues list

testAllValuesTheSame = do
  monadicDatabaseS $ do
    dtbse <- pick newSampleDatabase
    vals <- run dtbse
    if not $ null vals then do
      (lv,w) <- pick $ elements vals
      s <- run $ atomic $ readLVar lv
      assert ((s :: Int) == w)
    else
      return ()

instance Show (DatabaseS a b) where
  show a = "DatabaseS[contents not showable]"

instance Show (LVar Int) where
  show = show . refToInt . varToRef

newEmptyDatabase :: SafeCopy b => IO (Database b)
newEmptyDatabase = createTempDirectory "." "database" >>= open . (++"/") 

monadicDatabaseS :: SafeCopy b => PropertyM (DatabaseS b) a -> Property
monadicDatabaseS = monadic (\a -> monadicIO $ run $ newEmptyDatabase >>= flip runDatabaseS a)

                                  
                                  
--databaseTest :: PropertyM (DatabaseS Int) 


just :: Maybe a -> a
just (Just a) = a

main :: IO ()
main = do
  db <- open "./hallo/"
  runDatabaseS db $ addValues [1 :: Int,2,3,4,5,6,7,8,9,10] >> return ()
  
main2 :: IO Int
main2 = do
  db <- open "./hallo/"
  runDatabaseS db $ do
    ref <- getRef 20 
    lv <- loadRef (just ref)
    atomic $ readLVar lv