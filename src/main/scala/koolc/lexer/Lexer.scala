package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

/*
 *  For some comments on the Parser Combaintors see ParserCombinator.scala
 */
object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._
  import koolc.combinators.ParserCombinator
  import koolc.combinators.ParserCombinator._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    new Iterator[Token] {
      // Note that since we use Streams here, we do not have to keep the
      // complete input in memory, but instead can rely on only the tail being
      // present.
      // We need some state for the Iterator.
      var current: Stream[Char] = source.toStream
      var sentEOF = false
      var rowcol = (1, 0)

      def forwardCount(from: (Int, Int), by: Stream[Char]) =
        (rowcol /: by) {
          case ((row, col), '\n') => (row + 1, 0)
          case ((row, col), '\t') => (row, col + 4)
          case ((row, col), _) => (row, col + 1)
        }

      def hasNext = !sentEOF

      def next =
        KOOLlexer(current) match {
        case (Some((Some(token), _, _)), _) if token.kind == EOF => {
          sentEOF = true

          val (row, col) = rowcol
          (new Token(EOF)).setPos(f, Position.encode(row, col))
        }

        case (Some((None, rest, i)), _) => {
          rowcol = forwardCount(rowcol, current.take(i))
          current = rest

          next
        }

        case (Some((Some(token), rest, i)), _) => {
          val (row, col) = rowcol
          token.setPos(f, Position.encode(row, col))

          rowcol = forwardCount(rowcol, current.take(i))
          current = rest

          if (token.kind == BAD) {
            ctx.reporter.error("Lexer-Error, Unexpected Item.", token)
          }

          token
        }
      }
    }
  }

  type StringLexer = ParserCombinator[Char, Option[Token]]
  def const[a, b](x: => a)(y: b) = x
  def string(s: String) = items(s.toList)
  def token(k: TokenKind) = (const(Option(new Token(k))) _)

  val keywords: StringLexer = List[StringLexer](
    string(":")       >> token(COLON),
    string(";")       >> token(SEMICOLON),
    string(".")       >> token(DOT),
    string(",")       >> token(COMMA),
    string("=")       >> token(EQSIGN),
    string("==")      >> token(EQUALS),
    string("!")       >> token(BANG),
    string("(")       >> token(LPAREN),
    string(")")       >> token(RPAREN),
    string("[")       >> token(LBRACKET),
    string("]")       >> token(RBRACKET),
    string("{")       >> token(LBRACE),
    string("}")       >> token(RBRACE),
    string("&&")      >> token(AND),
    string("||")      >> token(OR),
    string("<")       >> token(LESSTHAN),
    string("+")       >> token(PLUS),
    string("-")       >> token(MINUS),
    string("*")       >> token(TIMES),
    string("/")       >> token(DIV),
    string("object")  >> token(OBJECT),
    string("class")   >> token(CLASS),
    string("def")     >> token(DEF),
    string("var")     >> token(VAR),
    string("Unit")    >> token(UNIT),
    string("main")    >> token(MAIN),
    string("String")  >> token(STRING),
    string("extends") >> token(EXTENDS),
    string("Int")     >> token(INT),
    string("Bool")    >> token(BOOLEAN),
    string("while")   >> token(WHILE),
    string("if")      >> token(IF),
    string("else")    >> token(ELSE),
    string("return")  >> token(RETURN),
    string("length")  >> token(LENGTH),
    string("true")    >> token(TRUE),
    string("false")   >> token(FALSE),
    string("this")    >> token(THIS),
    string("new")     >> token(NEW),
    string("println") >> token(PRINTLN)
  ).reduce(_ ||> _)

  val identifier: StringLexer =
    one[Char](x => x.isLetter) **
    star(one[Char](x => x.isLetterOrDigit || x == '_')) >>
    fuse >> {
      id => Some(new ID(id.mkString))
    }

  val integer: StringLexer =
    ((one[Char](x => x.isDigit && x != '0') ** star(one[Char](_.isDigit)) >> fuse) ||
    (item('0') >> { _ => List[Char]() })) >> {
      ds => (0 /: ds)(_ * 10 + _.asDigit)
    } >> {
      int => Some(new INTLIT(int))
    }

  val string: StringLexer = {
    // The strings here do allow for some escape sequences to be used in their
    // definition. Since this is not part of the language definition, we use an
    // empty escape map.
    //
    // val escapes = Map(
    //   """\n""" -> '\n',
    //   """\t""" -> '\t',
    //   """\"""" -> '\"',
    //   """\'""" -> '\'',
    //   """\\""" -> '\\'
    // ).map{case (f, t) => string(f) >> const(t)}.reduce(_ || _)

    val escapes = fail[Char, Char]

    def literal(quote: Char) = one[Char](!Set(quote, '\n').contains(_))

    val dualquote: StringLexer =
      enclosed('"', '"', star(escapes || literal('"'))) >> {
        str => Some(new STRLIT(str.mkString))
      }

    // Same for single-quote strings, which are not part of the language.
    // val singlequote: StringLexer =
    //   enclosed('\'', '\'', star(literal('\''))) >> {
    //     str => Some(new STRLIT(str.mkString))
    //   }
    val singlequote: StringLexer = fail

    // This consumes enough chars for broken strings to avoid parser errors.
    // This is due to the fact that the combinators cannot "skip" broken code.
    // While this might be possible to implement with the newer error semantic
    // I added for the parsers, it was not when I implemented this, so here is
    // a (not so nice) workaround.
    def badString(quote: Char): StringLexer =
      enclosedP(item(quote), item('\n') || (eof >> const('?')), star(literal(quote))) >>
      token(BAD)

    dualquote || singlequote || badString('"') || badString('\'')
  }

  val whitespace: StringLexer =
    plus(one[Char](_.isWhitespace)) >> const(None)

  val comment: StringLexer =
    // It is possible to open but not close a multiline comment, this just
    // means that everything until the end of the file will be discarded. If this
    // is not desired, simply remove the last parser, which will then however
    // lead to errors which are unexpected (since /* will be parsed as
    // token(DIV), token(MULT)).
    (
      enclosedP(string("//"), string("\n"), star(one[Char](_ != '\n'))) ||
      enclosedP(string("//"), eof, star(one[Char](_ != '\n'))) ||
      enclosedP(string("/*"), string("*/"), star(two[Char]((_, _) != ('*', '/')))) ||
      enclosedP(string("/*"), eof, star(one[Char](_ => true)))
    ) >> const(None)

  val KOOLlexer: StringLexer =
    // Note the longest-match combination of keywords and identifiers.
    // This allows identifiers which start with a keyword, like "var_test".
    (eof >> token(EOF)) || comment || whitespace ||
    (keywords ||> identifier) || integer || string ||
    (one[Char](_ => true) >> token(BAD))
}
