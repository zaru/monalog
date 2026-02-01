import Monalog.Markdown.ParserTest qualified
import Monalog.Markdown.RenderTest qualified
import Monalog.MarkdownTest qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ Monalog.MarkdownTest.tests,
        Monalog.Markdown.ParserTest.tests,
        Monalog.Markdown.RenderTest.tests
      ]
