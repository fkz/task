{-# LANGUAGE OverloadedStrings #-}
module HtmlTemplates where
import Text.Blaze.Html5 hiding (map, head, output)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A



formula :: Html -> Html
formula form = docTypeHtml $ do
  head [title $ "Standardtitel"]
  body form
  
tmplte :: Html -> Html
tmplte f = docTypeHtml $ do
  head [title "Standardtitel"]
  body f