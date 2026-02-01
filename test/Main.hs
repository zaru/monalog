import Monalog.Markdown.ParserTest qualified
import Monalog.Markdown.RenderTest qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ Monalog.Markdown.ParserTest.tests,
        Monalog.Markdown.RenderTest.tests
      ]
