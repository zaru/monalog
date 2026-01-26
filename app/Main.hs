module Main where

-- ライブラリのインポート時に関数を指定することができる
-- 指定しないとグローバルにすべて展開される
-- import qualified Foo as F と別名をつけることもできる
import Control.Monad (forever)
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as B
import Network.Socket
import Network.Socket.ByteString

-- ()はvoid相当、ここではなにも入ってないIOを返すmain関数である
main :: IO ()
-- bracketで確保したソケットをクローズする処理を定義できる
-- 関数定義 → bracket リソース確保 リソース解放 リソースを使った処理
-- \sockのバックスラッシュはラムダ式の始まりを表す do 式のサーバ処理をラムダとして渡している
main = withSocketsDo $ bracket (serveSocket 3000) close $ \sock -> do
  -- listenは指定ソケットで待機接続数を指定
  listen sock maxListenQueue
  -- memo: $は右側の式をすべて評価してから、左側の関数に渡す、カッコ () の代わり
  -- forever は無限ループする
  forever $ do
    (conn, addr) <- accept sock
    -- ++は文字列リストの結合、文字列は文字のリストなのでリスト同士を結合する
    -- showは .toString() 相当のもの、addr 変数を文字列に変換する
    putStrLn $ "接続: " ++ show addr

    -- リクエスト・メッセージを読み取る（読み取らないと即切断になりRecv failure: Connection reset by peerが出る）
    msg <- recv conn 1024
    print msg

    -- HTMLファイルを読み込む
    -- System IOのreadFileだと日本語が文字化する
    -- Data.ByteString.Char8だとバイトのままだ扱える
    body <- B.readFile "./html/index.html"
    -- 送信データサイズを計算する
    let len = B.length body
    -- ヘッダーを送信しないとブラウザで表示されない
    let header = B.pack $ "HTTP/1.1 200 OK\r\nContent-Length: " ++ show len ++ "\r\nContent-Type: text/html\r\n\r\n"
    -- sendAllでヘッダとボディのByteStringを結合して返す
    sendAll conn $ B.concat [header, body]

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
