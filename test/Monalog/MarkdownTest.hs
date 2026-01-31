{-# LANGUAGE OverloadedStrings #-}

module Monalog.MarkdownTest (tests) where

import Monalog.Markdown
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Markdown"
    [ testGroup
        "Markdown Tests"
        [ testCase "taggedCodeBlock - simple code block" $
            taggedCodeBlock ["```", "code", "```"]
              @?= ["<pre><code>code</code></pre>"],
          testCase "taggedCodeBlock - empty input" $
            taggedCodeBlock [] @?= [],
          testCase "taggedCodeBlock - no code block" $
            taggedCodeBlock ["## Title"]
              @?= ["<h2>Title</h2>"],
          testCase "taggedCodeBlock - full text" $
            taggedCodeBlock
              [ "## Title",
                "paragraph",
                "foo `code1` bar `code2`",
                "```",
                "code",
                "block",
                "```",
                "footer"
              ]
              @?= [ "<h2>Title</h2>",
                    "<p>paragraph</p>",
                    "<p>foo <code>code1</code> bar <code>code2</code></p>",
                    "<pre><code>code\nblock</code></pre>",
                    "<p>footer</p>"
                  ]
        ]
    ]
