package cq_challenge_markup

import scala.util.parsing.input.CharSequenceReader
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class LexerSuite extends FunSuite with ShouldMatchers {
  val lexer = new Lexer()
  import lexer._

  def firstTokenFrom(s: String) = token(new CharSequenceReader(s)).get

  def scanned(s: String) = {
    def tokens(scanner : Scanner) : List[Token] = 
      if (scanner.atEnd) List() else scanner.first :: tokens(scanner.rest)
    tokens(new Scanner(s))
  }

  test("asterisks followed by space are recognised as Asterisks token") {
    firstTokenFrom ("*** some asterisks") should equal (Asterisks(3))
  }

  test("asterisks with no space are recognised as Str token") {
    firstTokenFrom ("***some asterisks") should equal (Str("***some asterisks"))
  }

  test("text is scanned until double new line") {
    firstTokenFrom ("some sample text\n\nwith a double new line") should equal (Str("some sample text"))
  }

  test("lf is recognised as a new line token") {
    firstTokenFrom ("\n\nasdf") should equal (DoubleNewLine)
  }

  test("cr-lf is recognised as a new line token") {
    firstTokenFrom ("\r\n\r\nasdf") should equal (DoubleNewLine)
  }

  test("double new line with whitespace in between is recognised as double new line token") {
    firstTokenFrom ("\n \t \nsome text") should equal (DoubleNewLine)
  }

  test("tag start and end are recognised") {
    scanned ("\\tagname{tag content}") should equal (List(TagStart("tagname"), Str("tag content"), TagEnd))
  }

  test("simple ordered list is recognised") {
    scanned ("  # item 1\n  # item 2 with\n    new line in it\n  # item 3") should equal (
      List(ListHash, Str("item 1"), Str("\n"), 
	   ListHash, Str("item 2 with"), Str("\n"), Str("    new line in it"), Str("\n"),
	   ListHash, Str("item 3")))
  }

  val textToScan = """*** Header

paragraph 1,
      which consists of multiple
lines.
  
paragraph 2 with a \tag{ multi-paragraph

tag}"""

  test("scan some string") {
    //println(textToScan.replace("\n", "<lf>").replace("\r", "<cr>"))
    println(scanned(textToScan))
  }
}
