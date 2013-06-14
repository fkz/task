{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Html where

import Control.Applicative
import Heist
import Text.XmlHtml
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Heist
import Control.Lens
import Generate
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Text.Encoding

import Text.Blaze.Html5 hiding (map, head, output)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

data HtmlGenerate a = HtmlGenerate { _parameter :: Int, 
                                     _content :: [T.Text] -> Html, 
                                     _output :: [T.Text] -> Maybe a }

makeLenses ''HtmlGenerate

instance Functor HtmlGenerate where
  fmap f a = a & output %~ (\g h -> f <$> g h) 

instance Applicative HtmlGenerate where
  pure a = HtmlGenerate 0 (const (return ())) (const (Just a))
  (HtmlGenerate p1 str1 f1) <*> (HtmlGenerate p2 str2 x) = 
    HtmlGenerate (p1 + p2) 
    (\str -> let (a,b) = splitAt p1 str in str1 a >> str2 b)
    (\str -> let (a,b) = splitAt p1 str in (f1 a) <*> (x b))

node :: Html -> HtmlGenerate ()
node n = HtmlGenerate 0 (const n) (const (Just ()))

formData :: (T.Text -> Html) -> (T.Text -> Maybe a) -> HtmlGenerate a
formData f g = HtmlGenerate 1 (\a -> f (head a)) (\a -> g (head a))

instance BasicGenerator HtmlGenerate where
  message a = node $ p $ toHtml a
  getByName a b = flip formData (Just . T.unpack) (\s -> 
    H.label $ do 
      toHtml a   
      input ! name (toValue s) ! type_ "text" ! value (toValue b))
  mistake s = HtmlGenerate 0 (const $ return ()) (const Nothing)

data HtmlGenerateSnaplet b a = HtmlGenerateSnaplet {
  _heist :: Snaplet (Heist b) }

makeForm :: T.Text -> T.Text -> HtmlGenerate a -> Html
makeForm url key g = H.form ! A.method "post" ! action (toValue url) $ do
  input ! type_ "hidden" ! name "key" ! value (toValue key)  
  _content g (map (T.pack . ("t"++) . show) [1.._parameter g])
  input ! type_ "submit" ! value "Abschicken"

htmlCalculate :: HtmlGenerate a -> Request -> Maybe a
htmlCalculate gen req = do
   params <- sequence $ map getParam_ [1.._parameter gen]
   _output gen (map decodeUtf8 params) 
  where
    params = rqPostParams req
    singleton [] = Nothing
    singleton [a] = Just a
    singleton _ = Nothing
    getParam_ i = M.lookup (encodeUtf8 $ T.pack $ "t"++show i) params >>= singleton