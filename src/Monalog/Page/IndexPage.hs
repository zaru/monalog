{-# LANGUAGE OverloadedStrings #-}

module Monalog.Page.IndexPage (renderIndexPage) where

import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Maybe
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TO
import Monalog.Counter.Counter
import Monalog.Markdown.Parser
import Monalog.Markdown.Render
import Monalog.Page.RenderPage
import System.Directory

renderIndexPage :: IO ByteString
renderIndexPage = do
  files <- listAricleFile
  articles <- mapM renderArticle $ files
  count <- countup
  renderPage "./html/index.html" $ [("MAIN", T.concat articles), ("COUNTER", count)]

renderArticle :: String -> IO PageTitle
renderArticle filename = do
  mdFile <- TO.readFile $ "./data/" <> filename
  let (title, _) = parseMarkdownPage mdFile
  let path = "/a/" <> fromJust (T.stripSuffix ".md" $ T.pack filename)
  pure $ "<li><a href='" <> path <> "'>" <> title <> "</a></li>"

listAricleFile :: IO [String]
listAricleFile = sortOn Down . filter (`notElem` [".", ".."]) <$> getDirectoryContents "./data/"
