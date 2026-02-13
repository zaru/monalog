{-# LANGUAGE OverloadedStrings #-}

module Monalog.Markdown.Render where

import Data.Text (Text) -- Text型だけエイリアスなしで使えるようにする
import Data.Text qualified as T -- それ以外はPreludeとかぶらないようにエイリアス
import Monalog.Markdown.Parser

type Link = Text

renderHTML :: Markdown -> Text
renderHTML [] = ""
renderHTML markdown = T.concat $ map renderBlock markdown

renderBlock :: Block -> Text
renderBlock (Heading One line) = "<h1>" <> T.concat (map renderInline line) <> "</h1>"
renderBlock (Heading Two line) = "<h2>" <> T.concat (map renderInline line) <> "</h2>"
renderBlock (Paragraph line) = "<p>" <> T.concat (map renderInline line) <> "</p>"
renderBlock (CodeBlock line) = "<pre><code>" <> escapeSpecialChars (T.intercalate "\n" line) <> "</code></pre>"

renderInline :: Inline -> Text
renderInline (Plain line) = escapeSpecialChars line
renderInline (Code line) = "<code>" <> escapeSpecialChars line <> "</code>"
renderInline (Strong line) = "<strong>" <> T.concat (map renderInline line) <> "</strong>"
renderInline (Link (label, url)) = "<a href='" <> url <> "'>" <> escapeSpecialChars label <> "</a>"

escapeSpecialChars :: Text -> Text
escapeSpecialChars chars =
  T.replace "<" "&lt;" $
    T.replace ">" "&gt;" chars
