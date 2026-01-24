module Main where

-- ライブラリのインポート時に関数を指定することができる
-- 指定しないとグローバルにすべて展開される
-- import qualified Foo as F と別名をつけることもできる
import Control.Monad (forever)
import Network.Socket

main :: IO ()
main = do
  sock <- serveSocket 3000
  -- listenは指定ソケットで待機接続数を指定
  listen sock maxListenQueue
  -- memo: $は右側の式をすべて評価してから、左側の関数に渡す、カッコ () の代わり
  -- forever は無限ループする
  forever $ do
    (conn, addr) <- accept sock
    -- ++は文字列リストの結合、文字列は文字のリストなのでリスト同士を結合する
    -- showは .toString() 相当のもの、addr 変数を文字列に変換する
    putStrLn $ "接続: " ++ show addr
    close conn

serveSocket :: PortNumber -> IO Socket
serveSocket port = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet port (tupleToHostAddress (0, 0, 0, 0)))
  return sock
