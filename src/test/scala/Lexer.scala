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

  test("double new line tokens are recognised") {
    tokenFrom ("\n\n") should equal (lexer.DoubleNewLine)
  }

  test("string is scanned until double new line") {
    tokenFrom ("some sample text\n\nwith a new line") should equal (lexer.Str("some sample text"))
  }

  val textToScan = """
* Header
  
paragraph 1,
      which consists of multiple
lines.

paragraph 2 with \ a backslash
  """

  test("scan some string") {
    val scanner = new lexer.Scanner(textToScan)
    def printTokens(scanner : lexer.Scanner) : Unit = 
      if (!scanner.atEnd) {
	println(">>>>")
	println(scanner.first)
	println("<<<<")
	printTokens(scanner.rest)
      }
    printTokens(scanner)
  }
}
