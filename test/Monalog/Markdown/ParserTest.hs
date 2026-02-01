{-# LANGUAGE OverloadedStrings #-}

module Monalog.Markdown.ParserTest (tests) where

import Monalog.Markdown.Parser
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Markdown Parser"
    [ testCase "h2" $
        parseMarkdown "## head2"
          @?= [Heading Two [Plain "head2"]],
      testCase "p" $
        parseMarkdown "head2"
          @?= [Paragraph [Plain "head2"]],
      testCase "code block" $
        parseMarkdown "```\ncode\nblock\n```"
          @?= [CodeBlock ["code", "block"]],
      testCase "h2 code" $
        parseMarkdown "## head2 `code` text"
          @?= [Heading Two [Plain "head2 ", Code "code", Plain " text"]],
      testCase "p code" $
        parseMarkdown "p `code` text"
          @?= [Paragraph [Plain "p ", Code "code", Plain " text"]],
      testCase "full" $
        parseMarkdown "## Head\nparagraph\nfoo `code1` bar `code2`\n```\ncode\nblock\n```\nfooter"
          @?= [ Heading Two [Plain "Head"],
                Paragraph [Plain "paragraph"],
                Paragraph [Plain "foo ", Code "code1", Plain " bar ", Code "code2"],
                CodeBlock ["code", "block"],
                Paragraph [Plain "footer"]
              ]
    ]
