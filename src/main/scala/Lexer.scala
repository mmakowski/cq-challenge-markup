package cq_challenge_markup

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.token.Tokens

object MarkupSyntax {
  trait DocumentElement
  case class Document(elems: List[DocumentElement])
  trait ParagraphElement
  case class Paragraph(elems: List[ParagraphElement]) extends DocumentElement
  case class Text(s: String) extends ParagraphElement
  trait TagContents
  case class Tag(contents: TagContents) extends ParagraphElement
}

trait MarkupTokens extends Tokens {
  case class TagStart(id: String) extends Token { def chars = "\\" + id + "{" }
  case object DoubleNewLine extends Token { def chars = "\\" }
  case class Asterisks(length: Int) extends Token { def chars = "*" * length }
  case object TagEnd extends Token { def chars = "}" }
  case object ListHash extends Token { def chars = "  # " }
  case class Str(s: String) extends Token { def chars = s }
}

class Lexer extends Lexical with MarkupTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh

  def whitespace : Parser[Any] = success()

  def token : Parser[Token] = (
    asterisks
    | tagStart
    | '}' ^^^ TagEnd
    | ' '~' '~'#'~' ' ^^^ ListHash
    | doubleNewLine ^^^ DoubleNewLine
    | EofCh ^^^ EOF
    | rep1(chrExcept(EofCh, '\\', '\n', '\r', '}')) ^^ (charList => Str(charList mkString ""))
    | newLine ^^^ Str("\n")
  )

  def asterisks : Parser[Token] = rep1('*')<~' ' ^^ (astList => Asterisks(astList.length))
  def tagStart : Parser[Token] = '\\'~>rep1(chrExcept('{'))<~'{' ^^ (charList => TagStart(charList mkString ""))
  def doubleNewLine = newLine~whitespaceIgnorableBeforeNewLine~newLine
  def newLine = '\r'~'\n' | '\n'~'\r' | '\n' | '\r'
  def whitespaceIgnorableBeforeNewLine = rep(elem(' ') | '\t')
}

class Parser extends TokenParsers {
  type Tokens = MarkupTokens
  val lexical = new Lexer
  import lexical.{TagStart, Str}
  import MarkupSyntax._

  def document : Parser[Document] = rep(paragraph) ^^ 
				    (elts => Document(elts))
  def paragraph : Parser[DocumentElement] = rep1(paragraphElement) ^^ 
				   	    (elts => Paragraph(elts))
  def paragraphElement : Parser[ParagraphElement] = text
  def text : Parser[Text] = Str("dupa") ^^ (str => Text(str.asInstanceOf[Str].s))
  
}

