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

data HtmlGenerate a = HtmlGenerate { _parameter :: Int, 
                                     _content :: [T.Text] -> [Node], 
                                     _output :: [T.Text] -> Maybe a }

makeLenses ''HtmlGenerate

instance Functor HtmlGenerate where
  fmap f a = a & output %~ (\g h -> f <$> g h) 

instance Applicative HtmlGenerate where
  pure a = HtmlGenerate 0 (const []) (const (Just a))
  (HtmlGenerate p1 str1 f1) <*> (HtmlGenerate p2 str2 x) = 
    HtmlGenerate (p1 + p2) 
    (\str -> let (a,b) = splitAt p1 str in str1 a ++ str2 b)
    (\str -> let (a,b) = splitAt p1 str in (f1 a) <*> (x b))

node :: [Node] -> HtmlGenerate ()
node n = HtmlGenerate 0 (const n) (const (Just ()))

formData :: (T.Text -> [Node]) -> (T.Text -> Maybe a) -> HtmlGenerate a
formData f g = HtmlGenerate 1 (\a -> f (head a)) (\a -> g (head a))

instance BasicGenerator HtmlGenerate where
  message a = node ([TextNode (T.pack a)])
  getByName a b = flip formData (Just . T.unpack) (\s -> 
    [Element "label" [] [TextNode (T.pack a),   
                      Element "input" [("name",s),("type","text"),("value",T.pack b)] []]])
  mistake s = HtmlGenerate 0 (const []) (const Nothing)

data HtmlGenerateSnaplet b a = HtmlGenerateSnaplet {
  _heist :: Snaplet (Heist b) }

makeForm :: T.Text -> T.Text -> HtmlGenerate a -> [Node]
makeForm url key g = [Element "form" [("method","post"),("action",url)] ( 
                     Element "input" [("type","hidden"),("name","key"),("value",key)] [] : 
                     _content g (map (T.pack . ("t"++) . show) [1.._parameter g]) ++
                     [Element "input" [("type","submit"),("value","Abschicken")] []])]

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