-- GHC拡張で、リテラル文字列をStringだけでなくTextやByteStringで扱える
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- ライブラリのインポート時に関数を指定することができる
-- 指定しないとグローバルにすべて展開される
-- import qualified Foo as F と別名をつけることもできる

import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as TO
import Network.Socket
import Network.Socket.ByteString

-- ()はvoid相当、ここではなにも入ってないIOを返すmain関数である
main :: IO ()
-- bracketで確保したソケットをクローズする処理を定義できる
-- 関数定義 → bracket リソース確保 リソース解放 リソースを使った処理
-- \sockのバックスラッシュはラムダ式の始まりを表す do 式のサーバ処理をラムダとして渡している
main = withSocketsDo $ bracket (serveSocket 8000) close $ \sock -> do
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
    msg <- recv conn 1024
    print msg

    -- HTMLテンプレートとブログ記事Markdownを読み込む
    -- ByteStringだと文字列操作が煩雑なので、ByteStringを継承する？Textを利用する
    -- など、System IOのreadFileだと日本語が文字化する
    template <- TO.readFile "./html/index.html"
    mdFile <- TO.readFile "./html/index.md"
    -- Markdownをパースして文字列結合する
--    let body = T.concat $ tagged $ T.lines mdFile
    let body = T.concat $ taggedCodeBlock $ T.lines mdFile
    -- テンプレートを置き換え
    let html = T.replace "{__INDEX__}" body template
    -- 送信データサイズを計算する
    let len = T.show $ B.length $ encodeUtf8 html
    -- ヘッダーを送信しないとブラウザで表示されない
    let header = "HTTP/1.1 200 OK\r\nContent-Length: " <> len <> "\r\nContent-Type: text/html\r\n\r\n"
    -- sendAllでヘッダとボディのByteStringを結合して返す
    -- TextはText.encode.encodeUtf8でByteStringに変換できる
    sendAll conn $ encodeUtf8 $ T.concat [header, html]

    close conn

-- 超簡易Markdownパーサ
tagged :: T.Text -> T.Text
tagged x = case T.stripPrefix "## " x of
  -- prefixが見つかったら、前後を <h2> </h2> で挟む
  -- <code>対応しているがこういう愚直な呼び出し方で良いのだろうか？
  Just rest -> "<h2>" <> taggedCode (rest) <> "</h2>"
  -- 見つからなければ段落扱い
  Nothing -> "<p>" <> taggedCode (x) <> "</p>"

taggedCodeBlock :: [T.Text] -> [T.Text]
taggedCodeBlock [] = []
taggedCodeBlock (x : xs)
  | T.stripPrefix "```" x /= Nothing =
      let (block, rest) = break (T.isPrefixOf "```") xs
       in if length rest > 0
            then ["<pre><code>" <> T.intercalate "\n" block <> "</code></pre>"] ++ taggedCodeBlock (tail rest)
            else xs
  | otherwise = [tagged x] ++ taggedCodeBlock xs

-- <code>をラップするパーサ
-- 再帰処理をして頑張っているが…ダサい感じがある
taggedCode :: T.Text -> T.Text
taggedCode text
  -- \| のパイプでif-then-elseよりもシンプルに条件分岐できる
  -- 最後の = の右辺がリターンされる値
  | T.null text = ""
  -- otherwiseは | でガードを使った条件分岐の最後のケース
  | otherwise =
      let (left, right) = T.breakOn "`" text
       in -- let...in は let を一時的に使うための区切られたスコープ
          if T.null right
            then text
            else
              let right1 = T.drop 1 right
                  (code, rest) = T.breakOn "`" right1
               in if T.null rest
                    -- 閉じタグのバックティックが見つからないパターンのときはそのまま返す
                    then text
                    else
                      -- 最後のrestを再起処理し複数のcodeに対応
                      -- \$は優先順位が低いため () で囲って先にdrop処理する
                      left <> "<code>" <> code <> "</code>" <> taggedCode (T.drop 1 rest)

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
