{-# LANGUAGE OverloadedStrings #-}

module Monalog.Counter.Counter where

import Data.ByteString.Char8 qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Network.Connection
import System.Environment

countup :: IO Text
countup = do
  host <- getEnv "REDIS_HOST"
  port <- getEnv "REDIS_PORT"
  pass <- getEnv "REDIS_PASSWORD"
  appEnv <- getEnv "APP_ENV"

  -- 接続コンテキストの作成
  ctx <- initConnectionContext

  -- TLSの設定
  -- TLSSettingsSimple (証明書の検証) (ホスト名の検証) (セッション再利用)
  let tlsSettings = TLSSettingsSimple False False False
      connParams =
        ConnectionParams
          { connectionHostname = host,
            connectionPort = read port,
            connectionUseSecure = Just tlsSettings,
            connectionUseSocks = Nothing
          }

  conn <- connectTo ctx connParams

  -- 認証コマンド
  let authCmd = C.pack $ "*2\r\n$4\r\nAUTH\r\n$" ++ show (length pass) ++ "\r\n" ++ pass ++ "\r\n"
  connectionPut conn authCmd
  _authRes <- connectionGet conn 4096

  -- カウントアップ
  let counterKey = appEnv ++ "_counter"
  let incrCmd = "*2\r\n$4\r\nINCR\r\n$" ++ show (length counterKey) ++ "\r\n" ++ counterKey ++ "\r\n"
  connectionPut conn $ C.pack incrCmd
  response <- connectionGet conn 4096

  connectionClose conn

  -- レスポンスは :100\r\n という文字列になるので不要な文字を削除する
  let responseText = decodeUtf8 response
  pure $ T.dropEnd 2 $ T.drop 1 responseText
