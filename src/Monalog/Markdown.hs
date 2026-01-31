{-# LANGUAGE OverloadedStrings #-}

module Monalog.Markdown (taggedCodeBlock) where

import Data.Maybe (isJust)
import Data.Text qualified as T

-- 超簡易Markdownパーサ

-- コードブロック対応
taggedCodeBlock :: [T.Text] -> [T.Text]
taggedCodeBlock [] = []
taggedCodeBlock (x : xs)
  -- isJustを使うとMaybeが値を持っているかをチェックできる /= Nothing の比較より楽
  | isJust (T.stripPrefix "```" x) =
      let (block, rest) = break (T.isPrefixOf "```") xs
       in if not (null rest)
            -- 配列の結合は : もしくは ++ でやるが先頭追加なら : が良いらしい
            -- [1] : [2] ++ [3] → [1,2,3]
            then ("<pre><code>" <> T.intercalate "\n" block <> "</code></pre>") : taggedCodeBlock (tail rest)
            else xs
  | otherwise = taggedLine x : taggedCodeBlock xs

-- <h2>と<p>そしてインライン対応
taggedLine :: T.Text -> T.Text
taggedLine x = case T.stripPrefix "## " x of
  -- prefixが見つかったら、前後を <h2> </h2> で挟む
  -- <code>対応しているがこういう愚直な呼び出し方で良いのだろうか？
  Just rest -> "<h2>" <> taggedCode rest <> "</h2>"
  -- 見つからなければ段落扱い
  Nothing -> "<p>" <> taggedCode x <> "</p>"

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
