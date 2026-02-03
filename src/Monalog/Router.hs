{-# LANGUAGE OverloadedStrings #-}

module Monalog.Router (parseRequest, HttpRequest (..), HttpMethod (..)) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B -- ByteStringを扱いやすくするためにChar8にする

data HttpMethod = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS
  deriving (Show, Eq)

data HttpRequest = HttpRequest
  { method :: HttpMethod,
    path :: ByteString,
    version :: ByteString,
    headers :: [(ByteString, ByteString)]
  }
  deriving (Show, Eq)

-- パースを失敗する可能性があるのでMaybeにする
parseRequest :: ByteString -> Maybe HttpRequest
-- do式を使うことでMaybeのNothing対応を省略できる
-- 途中でNothingが発生したら、その時点で処理が終了
-- 使わない場合はcase..of Nothingの条件分岐が必要
parseRequest raw = do
  (reqLine, rest) <- splitLine raw
  (m, p, v) <- parseRequestLine reqLine
  pure
    HttpRequest
      { method = m,
        path = p,
        version = v,
        headers = parseHeaders rest
      }

parseRequestLine :: ByteString -> Maybe (HttpMethod, ByteString, ByteString)
parseRequestLine line = case B.words line of
  -- Applicative Style : Maybeの失敗するかも知れない文脈の中で関数適用させる書き方
  -- (,,) 3要素のタプルコンストラクタ関数
  -- <$> fmapのこと。リストに使えるmapをなんにでも使えるようにするもの、今回はMaybeな関数に適用している
  -- <*> applyのこと。MaybeとMaybeをくっつける
  -- pure Justと同じ。Maybeの生の値を抜き出す
  [m, p, v] -> (,,) <$> parseMethod m <*> pure p <*> pure v
  _ -> Nothing

parseMethod :: ByteString -> Maybe HttpMethod
parseMethod "GET" = Just GET
parseMethod "POST" = Just POST
parseMethod "PUT" = Just PUT
parseMethod "DELETE" = Just DELETE
parseMethod "PATCH" = Just PATCH
parseMethod "HEAD" = Just HEAD
parseMethod "OPTIONS" = Just OPTIONS
parseMethod _ = Nothing

parseHeaders :: ByteString -> [(ByteString, ByteString)]
parseHeaders bs = case splitLine bs of
  Nothing -> []
  Just ("", _) -> [] -- 空行 = ヘッダー終了
  Just (line, rest) -> case B.break (== ':') line of
    (key, val)
      | not (B.null val) ->
          -- Valueの先頭の : を削除したうえで、スペースを全部削除する
          -- 残り行数を再帰処理をする
          (key, B.dropWhile (== ' ') (B.tail val)) : parseHeaders rest
    _ -> parseHeaders rest

splitLine :: ByteString -> Maybe (ByteString, ByteString)
splitLine bs = case B.breakSubstring "\r\n" bs of
  (line, rest)
    | B.null rest -> Nothing
    -- 分割で先頭に残っている \r\n を drop して削除
    | otherwise -> Just (line, B.drop 2 rest)
