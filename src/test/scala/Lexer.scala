package cq_challenge_markup

import scala.util.parsing.input.CharSequenceReader
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class LexerSuite extends FunSuite with ShouldMatchers {
  val lexer = new Lexer()
  def firstTokenFrom(s: String) = lexer.token(new CharSequenceReader(s)).get

  test("asterisks followed by space are recognised as Asterisks token") {
    firstTokenFrom ("*** some asterisks") should equal (lexer.Asterisks(3))
  }

  test("asterisks with no space are recognised as Str token") {
    firstTokenFrom ("***some asterisks") should equal (lexer.Str("***some asterisks"))
  }

  test("backslash token is recognised") {
    firstTokenFrom ("\\") should equal (lexer.Backslash)
  }

  test("lf is recognised as a new line token") {
    firstTokenFrom ("\n\nasdf") should equal (lexer.DoubleNewLine)
  }

  test("cr-lf is recognised as a new line token") {
    firstTokenFrom ("\r\n\r\nasdf") should equal (lexer.DoubleNewLine)
  }

  test("double new line with whitespace in between is recognised as double new line token") {
    firstTokenFrom ("\n \t \nsome text") should equal (lexer.DoubleNewLine)
  }

  test("string is scanned until double new line") {
    firstTokenFrom ("some sample text\n\nwith a double new line") should equal (lexer.Str("some sample text"))
  }
/*
  test("single new line is included in string") {
    firstTokenFrom ("some sample text\nwith a single new line") should equal (lexer.Str("some sample text\nwith a single new line"))
  }
*/
  val textToScan = """*** Header

paragraph 1,
      which consists of multiple
lines.
  
paragraph 2 with a \tag{ multi-paragraph

tag}"""

  test("scan some string") {
    //println(textToScan.replace("\n", "<lf>").replace("\r", "<cr>"))
    val scanner = new lexer.Scanner(textToScan)
    def tokens(scanner : lexer.Scanner) : List[lexer.Token] = 
      if (scanner.atEnd) List() else scanner.first :: tokens(scanner.rest)
    println(tokens(scanner))
  }
}
