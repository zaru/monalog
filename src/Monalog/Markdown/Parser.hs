{-# LANGUAGE OverloadedStrings #-}

-- Markdownの型をエクスポート
-- (..)で指定型のすべてのコンストラクタをエクスポートできる
module Monalog.Markdown.Parser (parseMarkdown, Markdown, Block (..), Inline (..), Level (..)) where

import Data.Text (Text) -- Text型だけエイリアスなしで使えるようにする
import Data.Text qualified as T -- それ以外はPreludeとかぶらないようにエイリアス
import Data.Maybe
import Control.Monad

data Inline
  = Plain Text
  | Code Text
  | Strong [Inline] -- 再帰にして<strong>内に他のInlineを埋め込めるようにする
  | Link (Text, Text)
  deriving (Show, Eq)

data Level = One | Two deriving (Show, Eq)

data Block
  = Heading Level [Inline]
  | Paragraph [Inline]
  | CodeBlock [Text]
  deriving (Show, Eq)

type Markdown = [Block]

parseMarkdown :: Text -> Markdown
parseMarkdown input = parseLines $ T.lines input

parseLines :: [Text] -> Markdown
parseLines [] = []
parseLines (currentLine : restLines)
  | T.isPrefixOf "```" currentLine =
      case break (T.isPrefixOf "```") restLines of
        (_, []) -> [Paragraph [Plain currentLine]]
        (block, rest) -> CodeBlock block : parseLines (tail rest)
  | T.isPrefixOf "## " currentLine = Heading Two (parseInline $ T.drop 3 currentLine) : parseLines restLines
  | otherwise = Paragraph (parseInline currentLine) : parseLines restLines

-- Markdown特殊文字判定
isSpecialChar :: Char -> Bool
isSpecialChar c = c == '`' || c == '*' || c == '['

-- 1行テキストをパースする
parseInline :: Text -> [Inline]
parseInline text
  | T.null text = []
  | otherwise =
      let (plain, rest) = T.break isSpecialChar text
       in ([Plain plain | not (T.null plain)])
            ++ parseSpecial rest

-- Markdown特殊文字で始まるテキストがわたる
parseSpecial :: Text -> [Inline]
parseSpecial text =
  case T.uncons text of
    Nothing -> []
    Just (c, next) ->
      case c of
        '`' -> parseCode text
        '*' -> [Strong [Plain text]] -- TODO
        '[' -> parseLink text
        _ -> Plain (T.singleton c) : parseInline next

-- ` 始まりのテキストがわたる
parseCode :: Text -> [Inline]
parseCode text =
  case T.breakOn "`" (T.drop 1 text) of
    (_, "") -> [Plain text] -- 閉じタグなしなのでPlain
    (code, next) -> Code code : parseInline (T.drop 1 next)

parseLink :: Text -> [Inline]
parseLink text = fromMaybe [Plain text] $ do
  (label, afterLabel) <- extractLinkLabel text
  (url, rest) <- extractLinkUrl afterLabel
  pure $ Link (label, url) : parseInline (T.drop 1 rest)

extractLinkLabel :: Text -> Maybe (Text, Text)
extractLinkLabel text = do
  let (label, next) = T.breakOn "]" (T.drop 1 text)
  guard (not $ T.null next)
  pure (label, next)

extractLinkUrl :: Text -> Maybe (Text, Text)
extractLinkUrl text = do
  next <- T.stripPrefix "(" (T.drop 1 text)
  let (url, rest) = T.breakOn ")" next
  guard (not $ T.null rest)
  pure (url, rest)

-- parseMarkdown "## h2\np1\np2`code1`bar`code2`desu\n```\nthis\nis block\n```\nlast"
