{-# LANGUAGE OverloadedStrings #-}

module Monalog.Router (parseRequest, Request (..), Method (..)) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC

data Method = Get deriving (Show, Eq)

data Request = NewRequest
  { method :: Method,
    path :: ByteString
  }
  deriving (Show, Eq)

parseRequest :: ByteString -> Request
parseRequest line =
  let (m, p) = parseMethod line
  in NewRequest m p

parseMethod :: ByteString -> (Method, ByteString)
parseMethod line =
  case BC.break (== ' ') line of
    (method, rest)
      | method == "GET" -> (Get, parsePath cleanRest)
      | otherwise -> (Get, parsePath cleanRest)
      where
        cleanRest = BC.dropWhile (== ' ') rest

parsePath :: ByteString -> ByteString
-- 合成を使って書くことができる
-- parsePath line = fst $ BC.break (== '?') $ fst $ BC.break (== ' ') line
parsePath = fst . BC.break (== '?') . fst . BC.break (== ' ')

request :: Method -> Request
request Get =
  NewRequest
    { method = Get,
      path = "/"
    }
