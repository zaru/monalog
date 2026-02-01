{-# LANGUAGE OverloadedStrings #-}

module Monalog.Markdown.Render where

import Data.Text (Text) -- Text型だけエイリアスなしで使えるようにする
import Data.Text qualified as T -- それ以外はPreludeとかぶらないようにエイリアス
import Monalog.Markdown.Parser

renderHTML :: Markdown -> Text
renderHTML [] = ""
renderHTML markdown = T.concat $ map renderBlock markdown

renderBlock :: Block -> Text
renderBlock (Heading One line) = "<h1>" <> T.concat (map renderInline line) <> "</h1>"
renderBlock (Heading Two line) = "<h2>" <> T.concat (map renderInline line) <> "</h2>"
renderBlock (Paragraph line) = "<p>" <> T.concat (map renderInline line) <> "</p>"
renderBlock (CodeBlock line) = "<pre><code>" <> T.intercalate "\n" line <> "</code></pre>"

renderInline :: Inline -> Text
renderInline (Plain line) = line
renderInline (Code line) = "<code>" <> line <> "</code>"
renderInline (Strong line) = "<strong>" <> T.concat (map renderInline line) <> "</strong>"
