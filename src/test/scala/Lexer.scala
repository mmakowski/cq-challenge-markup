package cq_challenge_markup

import scala.util.parsing.input.CharSequenceReader
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class LexerSuite extends FunSuite with ShouldMatchers {
  val lexer = new Lexer()
  def tokenFrom(s: String) = lexer.token(new CharSequenceReader(s)).get

  test("backslash token is recognised") {
    tokenFrom ("\\") should equal (lexer.Backslash)
  }

  test("lf is recognised as a new line token") {
    tokenFrom ("\nasdf") should equal (lexer.NewLine)
  }

  test("cr-lf is recognised as a new line token") {
    tokenFrom ("\r\nasdf") should equal (lexer.NewLine)
  }

  test("string is scanned until new line") {
    tokenFrom ("some sample text\nwith a new line") should equal (lexer.Str("some sample text"))
  }

  val textToScan = """* Header

paragraph 1,
      which consists of multiple
lines.

paragraph 2 with \ a backslash
  """

  test("scan some string") {
    //println(textToScan.replace("\n", "<lf>").replace("\r", "<cr>"))
    val scanner = new lexer.Scanner(textToScan)
    def tokens(scanner : lexer.Scanner) : List[lexer.Token] = 
      if (scanner.atEnd) List() else scanner.first :: tokens(scanner.rest)
    println(tokens(scanner))
  }
}
