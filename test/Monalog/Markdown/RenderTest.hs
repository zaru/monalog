{-# LANGUAGE OverloadedStrings #-}

module Monalog.Markdown.RenderTest (tests) where

import Monalog.Markdown.Parser
import Monalog.Markdown.Render
import Test.Tasty
import Test.Tasty.HUnit

-- cabal test --test-show-details=direct --test-option='-p' --test-option='/Render/'

tests :: TestTree
tests =
  testGroup
    "Markdown Render"
    [ testCase "empty" $
        renderHTML Nothing []
          @?= "",
      testCase "h2" $
        renderHTML Nothing [Heading Two [Plain "head2"]]
          @?= "<h2>head2</h2>",
      testCase "p" $
        renderHTML Nothing [Paragraph [Plain "paragraph"]]
          @?= "<p>paragraph</p>",
      testCase "code block" $
        renderHTML Nothing [CodeBlock ["code", "block"]]
          @?= "<pre><code>code\nblock</code></pre>",
      testCase "h2 code" $
        renderHTML Nothing [Heading Two [Plain "paragraph", Code "code"]]
          @?= "<h2>paragraph<code>code</code></h2>",
      testCase "p code" $
        renderHTML Nothing [Paragraph [Plain "paragraph", Code "code"]]
          @?= "<p>paragraph<code>code</code></p>",
      testCase "escape" $
        renderHTML Nothing [Paragraph [Plain "<>"]]
          @?= "<p>&lt;&gt;</p>",
      testCase "full" $
        renderHTML
          Nothing
          [ Heading Two [Plain "Head"],
            Paragraph [Plain "paragraph"],
            Paragraph [Plain "foo ", Code "code1", Plain " bar ", Code "code2"],
            CodeBlock ["code", "block"],
            Paragraph [Plain "footer"]
          ]
          @?= "<h2>Head</h2><p>paragraph</p><p>foo <code>code1</code> bar <code>code2</code></p><pre><code>code\nblock</code></pre><p>footer</p>"
    ]
