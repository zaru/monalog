-- GHC拡張で、リテラル文字列をStringだけでなくTextやByteStringで扱える
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- ライブラリのインポート時に関数を指定することができる
-- 指定しないとグローバルにすべて展開される
-- import qualified Foo as F と別名をつけることもできる

import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as B
import Monalog.Page.ArticlePage
import Monalog.Page.IndexPage
import Monalog.Page.RenderPage
import Monalog.Router
import Network.Socket
import Network.Socket.ByteString

-- ()はvoid相当、ここではなにも入ってないIOを返すmain関数である
main :: IO ()
-- bracketで確保したソケットをクローズする処理を定義できる
-- 関数定義 → bracket リソース確保 リソース解放 リソースを使った処理
-- \sockのバックスラッシュはラムダ式の始まりを表す do 式のサーバ処理をラムダとして渡している
main = withSocketsDo $ bracket (serveSocket 8000) close $ \sock -> do
  host <- getEnv "REDIS_HOST"
  port <- getEnv "REDIS_PORT"
  pass <- getEnv "REDIS_PASSWORD"
  appEnv <- getEnv "APP_ENV"
  putStrLn host
  putStrLn port
  putStrLn pass
  putStrLn appEnv

  -- listenは指定ソケットで待機接続数を指定
  listen sock maxListenQueue
  putStrLn "Starting Monalog http://localhost:8000"
  -- memo: $は右側の式をすべて評価してから、左側の関数に渡す、カッコ () の代わり
  -- forever は無限ループする
  forever $ do
    (conn, addr) <- accept sock
    -- ++は文字列リストの結合、文字列は文字のリストなのでリスト同士を結合する
    -- showは .toString() 相当のもの、addr 変数を文字列に変換する
    putStrLn $ "接続: " ++ show addr

    -- リクエスト・メッセージを読み取る（読み取らないと即切断になりRecv failure: Connection reset by peerが出る）
    requestData <- recv conn 1024
    print requestData

    -- ルーティング処理
    responseData <- case parseRequest requestData of
      Nothing -> renderPage "./html/index.html" [("MAIN", "Not Found")]
      Just r
        -- レコードフィールドの呼び出しはgetter関数が自動生成される
        -- つまり path r は path 関数に r を引数で渡している
        | path r == "/" -> renderIndexPage
        | "/a/" `B.isPrefixOf` path r -> renderArticlePage (path r)
        | otherwise -> renderPage "./html/index.html" [("MAIN", "Not Found")]
    sendAll conn responseData
    close conn

-- PortNumberを引数とし、Scoket型のIOコンストラクタを返す関数定義
serveSocket :: PortNumber -> IO Socket
serveSocket port = do
  -- AF_INETはrequestパッケージのIPv4定義
  -- Streamはrequestパッケージのデータ方式（ストリームはTCP）
  -- defaultProtocolはrequestパッケージのプロトコル番号（OSにお任せ）
  sock <- socket AF_INET Stream defaultProtocol
  -- SO_REUSEADDRを設定してポートを即座に再利用可能にする
  -- これを設定するとTIME_WAITであっても即座に再利用できるらしい
  setSocketOption sock ReuseAddr 1
  -- 作成したSocketをポートにバインドする
  -- ホストアドレスは文字列では指定できず、tupleToHostAddressを使ってタプル指定する
  bind sock (SockAddrInet port (tupleToHostAddress (0, 0, 0, 0)))
  return sock
