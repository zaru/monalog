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
      -- TODO: 今後のためにタイトルと本文を分けてパースする処理を追加している
      -- TODO: 次は、リンク記法をサポートしてタイトルへのリンクをMarkdownとして扱う
      -- let (title, content) = parseMarkdownPage mdFile
      -- let main = renderHTML Nothing [title] <> renderHTML Nothing content
      let (title, body) = parseMarkdownPage mdFile
      let content = renderHTML body
      count <- countup
      renderPage "./html/article.html" [("TITLE", escapeSpecialChars title), ("MAIN", content), ("COUNTER", count)]

extractArticleId :: ByteString -> Maybe ByteString
extractArticleId = B.stripPrefix "/a/"
