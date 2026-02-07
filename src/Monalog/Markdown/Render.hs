{-# LANGUAGE OverloadedStrings #-}

module Monalog.Markdown.Render where

import Data.Text (Text) -- Text型だけエイリアスなしで使えるようにする
import Data.Text qualified as T -- それ以外はPreludeとかぶらないようにエイリアス
import Monalog.Markdown.Parser

type Link = Text

renderHTML :: Maybe Link -> Markdown -> Text
renderHTML _ [] = ""
renderHTML Nothing markdown = T.concat $ map (renderBlock Nothing) markdown
renderHTML (Just link) markdown = T.concat $ map (renderBlock $ Just link) markdown

renderBlock :: Maybe Link -> Block -> Text
renderBlock _ (Heading One line) = "<h1>" <> T.concat (map renderInline line) <> "</h1>"
renderBlock Nothing (Heading Two line) = "<h2>" <> T.concat (map renderInline line) <> "</h2>"
renderBlock (Just link) (Heading Two line) = "<h2><a href='" <> link <> "'>" <> T.concat (map renderInline line) <> "</a></h2>"
renderBlock _ (Paragraph line) = "<p>" <> T.concat (map renderInline line) <> "</p>"
renderBlock _ (CodeBlock line) = "<pre><code>" <> escapeSpecialChars (T.intercalate "\n" line) <> "</code></pre>"

renderInline :: Inline -> Text
renderInline (Plain line) = escapeSpecialChars line
renderInline (Code line) = "<code>" <> escapeSpecialChars line <> "</code>"
renderInline (Strong line) = "<strong>" <> T.concat (map renderInline line) <> "</strong>"

escapeSpecialChars :: Text -> Text
escapeSpecialChars chars =
  T.replace "<" "&lt;" $
    T.replace ">" "&gt;" chars
