import Monalog.MarkdownTest qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [Monalog.MarkdownTest.tests]
