{-# LANGUAGE OverloadedStrings #-}

module Monalog.Page.RenderPage (renderPage) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as TO
import Monalog.Markdown.Parser
import Monalog.Markdown.Render

renderPage :: Text -> IO ByteString
renderPage content = do
  -- HTMLテンプレートとブログ記事Markdownを読み込む
  -- ByteStringだと文字列操作が煩雑なので、ByteStringを継承する？Textを利用する
  -- など、System IOのreadFileだと日本語が文字化する
  template <- TO.readFile "./html/index.html"
  -- テンプレートを置き換え
  let html = T.replace "{__INDEX__}" content template
  -- 送信データサイズを計算する
  let len = T.show $ B.length $ encodeUtf8 html
  -- ヘッダーを送信しないとブラウザで表示されない
  let header = "HTTP/1.1 200 OK\r\nContent-Length: " <> len <> "\r\nContent-Type: text/html\r\n\r\n"
  -- sendAllでヘッダとボディのByteStringを結合して返す
  -- TextはText.encode.encodeUtf8でByteStringに変換できる
  pure $ encodeUtf8 $ T.concat [header, html]
