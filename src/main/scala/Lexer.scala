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
  case object Backslash extends Token { def chars = "\\" }
  case object DoubleNewLine extends Token { def chars = "\n\n" }
  case class Str(s: String) extends Token { def chars = s }
}

class Lexer extends Lexical with MarkupTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh

  def whitespace : Parser[Any] = rep('\t')

  def token : Parser[Token] = (
      '\\' ^^^ Backslash
    | '\n'~'\n' ^^^ DoubleNewLine
    | EofCh ^^^ EOF
    | rep1(chrExcept(EofCh)) ^^ (charList => Str(charList mkString ""))
  )
}

class Parser extends TokenParsers {
  type Tokens = MarkupTokens
  val lexical = new Lexer
  import lexical.{Backslash, DoubleNewLine, Str}
  import MarkupSyntax._

  def document : Parser[Document] = rep(paragraph) ^^ 
				    (elts => Document(elts))
  def paragraph : Parser[DocumentElement] = rep1(paragraphElement) <~ DoubleNewLine ^^ 
				   	    (elts => Paragraph(elts))
  def paragraphElement : Parser[ParagraphElement] = text
  def text : Parser[Text] = Str("dupa") ^^ (str => Text(str.asInstanceOf[Str].s))
  
}

