{-# LANGUAGE OverloadedStrings #-}

module Monalog.Page.IndexPage (renderIndexPage) where

import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TO
import Monalog.Markdown.Parser
import Monalog.Markdown.Render
import Monalog.Page.RenderPage
import System.Directory

renderIndexPage :: IO ByteString
renderIndexPage = do
  files <- listAricleFile
  articles <- mapM renderArticle files
  renderPage "./html/index.html" $ T.concat articles

renderArticle :: String -> IO Text
renderArticle filename = do
  mdFile <- TO.readFile $ "./data/" <> filename
  pure $ renderHTML $ parseMarkdown mdFile

listAricleFile :: IO [String]
listAricleFile = sortOn Down . filter (`notElem` [".", ".."]) <$> getDirectoryContents "./data/"
