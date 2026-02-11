{-# LANGUAGE OverloadedStrings #-}

module Monalog.Page.ArticlePage (renderArticlePage) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as TO
import Monalog.Counter.Counter
import Monalog.Markdown.Parser
import Monalog.Markdown.Render
import Monalog.Page.RenderPage

renderArticlePage :: ByteString -> IO ByteString
renderArticlePage path = do
  case extractArticleId path of
    Nothing -> renderPage "./html/index.html" [("MAIN", "Not Found")]
    Just articleId -> do
      mdFile <- TO.readFile $ T.unpack ("./data/" <> decodeUtf8 articleId <> ".md")
      let content = renderHTML Nothing $ parseMarkdown mdFile
      count <- countup
      renderPage "./html/article.html" [("MAIN", content), ("COUNTER", count)]

extractArticleId :: ByteString -> Maybe ByteString
extractArticleId = B.stripPrefix "/a/"
