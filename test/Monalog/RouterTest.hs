{-# LANGUAGE OverloadedStrings #-}

module Monalog.RouterTest (tests) where

import Data.Text.Encoding
import Monalog.Router
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Router parseRequest"
    [ testCase "Top Page" $
        parseRequest (encodeUtf8 "GET / HTTP")
          @?= NewRequest
            { method = Get,
              path = "/"
            },
      testCase "Article Page" $
        parseRequest (encodeUtf8 "GET /s/202601011223 HTTP")
          @?= NewRequest
            { method = Get,
              path = "/s/202601011223"
            }
    ]
