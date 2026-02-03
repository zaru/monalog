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
        parseRequest (encodeUtf8 "GET / HTTP/1.1\r\n")
          @?= Just
            HttpRequest
              { method = GET,
                path = encodeUtf8 "/",
                version = encodeUtf8 "HTTP/1.1",
                headers = []
              },
      testCase "Article Page" $
        parseRequest (encodeUtf8 "GET /s/202601011223 HTTP/1.1\r\n")
          @?= Just
            HttpRequest
              { method = GET,
                path = encodeUtf8 "/s/202601011223",
                version = encodeUtf8 "HTTP/1.1",
                headers = []
              },
      testCase "Invalid" $
        parseRequest (encodeUtf8 "INVALID")
          @?= Nothing
    ]
