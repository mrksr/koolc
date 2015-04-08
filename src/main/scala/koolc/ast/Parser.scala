package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

/*
 *  For some comments on the Parser Combaintors see ParserCombinator.scala
 */
object Parser extends Pipeline[Iterator[Token], Program] {
  import koolc.combinators.ParserCombinator
  import koolc.combinators.ParserCombinator._
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    val tokenStream = tokens.toStream.filter(_.kind != BAD)
    val tree = GoalP(tokenStream)
    val program = tree match {
      case (Some((prog, _, _)), _) => Some(prog)
      case (None, Some((nms, i))) => {
        val token = tokenStream(i)
        fatal("Expected: %s, found: %s".format(nms.map(_.name).mkString(" or "), token), token)
        None
      }
      case (None, None) => {
        fatal("Parsing failed without a specific error.");
        None
      }
    }
    terminateIfErrors
    program.get
  }

  type Parser[A] = ParserCombinator[Token, A]
  def token(kind: TokenKind) = named(kind.toString)(one[Token]( t => t.kind == kind ))
  def const[a, b](x: => a)(y: b) = x

  /*
   * This parser is used to add a position to most tree elements created. While
   * p creates an "unpositioned" tree element, the head parser simply matches
   * on the first token (whichever it is) but does no consume it to forward the
   * position.
   */
  def positioned[T <: Tree](p: Parser[T]) =
    head ** p >> {
      case (token, tree) => tree.setPos(token.get)
    }

  // I wonder if this is really necessary, I do not quite get polymorphism in Scala.
  def as[A](t: Token) = t.asInstanceOf[A]

  /*****************
   *  Identifier  *
   ****************/
  val IdentifierP =
    positioned(
      token(IDKIND) >> as[ID] >> { id => Identifier(id.value) }
    )

  /*****************
   *  Expression  *
   ****************/
  val ExpressionP: Parser[ExprTree] = {
    val IntegerP: Parser[ExprTree] =
      positioned(
        token(INTLITKIND) >> as[INTLIT] >> { int => IntLit(int.value) }
      )

    val StringP: Parser[ExprTree] =
      positioned(
        token(STRLITKIND) >> as[STRLIT] >> { str => StringLit(str.value) }
      )

    val LiteralExpressionP: Parser[ExprTree] =
      List[Parser[ExprTree]] (
        positioned(token(TRUE)  >> const(True())),
        positioned(token(FALSE) >> const(False())),
        positioned(token(THIS)  >> const(This())),
        IdentifierP  >> { _.asInstanceOf[ExprTree] },
        StringP,
        IntegerP
      ).reduce(_ || _)

    val NewP: Parser[ExprTree] =
      positioned(
        token(NEW) *> IdentifierP *< token(LPAREN) *< token(RPAREN) >> {
          id => New(id)
        }
      )

    val NewIntArrayP: Parser[ExprTree] =
      positioned(
        token(NEW) *> token(INT) *> token(LBRACKET) *> ExpressionP *< token(RBRACKET) >> {
          exp => NewIntArray(exp)
        }
      )

    val NotP: Parser[ExprTree] =
      positioned(
        token(BANG) *> ExpressionP >> { exp => Not(exp) }
      )

    val BraceP: Parser[ExprTree] =
      token(LPAREN) *> ExpressionP *< token(RPAREN)

    val NoHeadRecursiveP =
      LiteralExpressionP || NewP || NewIntArrayP || NotP || BraceP

    val MethodReadLengthP: Parser[ExprTree] = {
      val LengthP: Parser[ExprTree => ExprTree] =
          token(DOT) *< token(LENGTH) >> {
            _ => exp => ArrayLength(exp)
          }

      val MethodP: Parser[ExprTree => ExprTree] =
          token(DOT) *> IdentifierP *< token(LPAREN) **
          optional(ExpressionP ** star(token(COMMA) *> ExpressionP) >> fuse) *< token(RPAREN) >> {
            case (method, args) => obj => MethodCall(obj, method, args.getOrElse(Nil))
          }

      val ReadP: Parser[ExprTree => ExprTree] =
          token(LBRACKET) *> ExpressionP *< token(RBRACKET) >> {
            idx => arr => ArrayRead(arr, idx)
          }

      head ** NoHeadRecursiveP ** star(LengthP || MethodP || ReadP) >> {
        case ((token, inner), apx) => (inner /: apx) { case (i, a) => a(i).setPos(token.get) }
      }
    }

    val OperatorP: Parser[ExprTree] = {
      // High to Low precedence
      type OpType = (ExprTree, ExprTree) => ExprTree
      val ops = List(
        Map[TokenKind, OpType](
          (TIMES, Times), (DIV, Div)
        ),
        Map[TokenKind, OpType](
          (PLUS, Plus),
          (MINUS, Minus)
        ),
        Map[TokenKind, OpType](
          (EQUALS, Equals),
          (LESSTHAN, LessThan)
        ),
        Map[TokenKind, OpType](
          (AND, And)
        ),
        Map[TokenKind, OpType](
          (OR, Or)
        )
      )

      (MethodReadLengthP /: ops) {
        case (inner, tkns) => {
          def outer: Parser[ExprTree] = {
            val opP = tkns.keys.map(token).reduce(_ || _)
            inner ** optional(head ** opP ** outer) >> {
              case (i, None) => i
              case (i, Some(((pos, op), o))) => tkns(op.kind)(i, o).setPos(pos.get)
            }
          }

          outer
        }
      }
    }

    OperatorP
  }

  /****************
   *  Statement  *
   ***************/
  val StatementP: Parser[StatTree] = {
    val AssignP: Parser[StatTree] =
      positioned(
        IdentifierP *< token(EQSIGN) ** ExpressionP *< token(SEMICOLON) >> {
          case (id, expr) => Assign(id, expr)
        }
      )

    val ArrayAssignP: Parser[StatTree] =
      positioned(
        IdentifierP *< token(LBRACKET) ** ExpressionP *< token(RBRACKET) *<
        token(EQSIGN) ** ExpressionP *< token(SEMICOLON) >> {
          case ((id, idx), expr) => ArrayAssign(id, idx, expr)
        }
      )

    val PrintlnP: Parser[StatTree] =
      positioned(
        token(PRINTLN) *> token(LPAREN) *> ExpressionP *< token(RPAREN) *< token(SEMICOLON) >> {
          exp => Println(exp)
        }
      )

    val WhileP: Parser[StatTree] =
      positioned(
        token(WHILE) *> token(LPAREN) *> ExpressionP *< token(RPAREN) ** StatementP >> {
          case (exp, stat) => While(exp, stat)
        }
      )

    val IfP: Parser[StatTree] =
      positioned(
        token(IF) *> token(LPAREN) *> ExpressionP *< token(RPAREN) ** StatementP **
        optional(token(ELSE) *> StatementP) >> {
          case ((exp, thn), els) => If(exp, thn, els)
        }
      )

    val BlockP: Parser[StatTree] =
      positioned(
        token(LBRACE) *> star(StatementP) *< token(RBRACE) >> {
          stats => Block(stats)
        }
      )

    List[Parser[StatTree]](
      AssignP,
      ArrayAssignP,
      PrintlnP,
      WhileP,
      IfP,
      BlockP
    ).reduce(_ || _)
  }

  /*******************
   *  Declarations  *
   ******************/
  val TypeP: Parser[TypeTree] =
    List[Parser[TypeTree]](
      positioned(token(BOOLEAN) >> const(BooleanType())),
      positioned(token(STRING)  >> const(StringType())),
      positioned(token(INT) ** token(LBRACKET) ** token(RBRACKET) >> const(IntArrayType())),
      positioned(token(INT) >> const(IntType())),
      IdentifierP >> { _.asInstanceOf[TypeTree] }
    ).reduce(_ || _)

  val VarP =
    positioned(
      token(VAR) *> IdentifierP *< token(COLON) ** TypeP *< token(SEMICOLON) >> {
        case (id, tpe) => VarDecl(tpe, id)
      }
    )

  val MethodDeclP = {
    val ArgP =
      positioned(
        IdentifierP *< token(COLON) ** TypeP >> {
          case (id, tpe) => Formal(tpe, id)
        }
      )

    positioned(
      token(DEF) *> IdentifierP *< token(LPAREN) **
      optional(ArgP ** star(token(COMMA) *> ArgP) >> fuse) *<
      token(RPAREN) *< token(COLON) ** TypeP *<
      token(EQSIGN) *< token(LBRACE) **
      star(VarP) ** star(StatementP) *<
      token(RETURN) ** ExpressionP *< token(SEMICOLON) *< token(RBRACE) >> {
        case (((((id, args), retType), vars), stats), retExpr) =>
          MethodDecl(retType, id, args.getOrElse(Nil), vars, stats, retExpr)
      }
    )
  }

  val ClassDeclP =
    positioned(
      token(CLASS) *> IdentifierP **
      optional(token(EXTENDS) *> IdentifierP) *< token(LBRACE) **
      star(VarP) ** star(MethodDeclP) *< token(RBRACE) >> {
        case (((id, parent), vars), methods) => ClassDecl(id, parent, vars, methods)
      }
    )

  val MainObjectP =
    positioned(
      token(OBJECT) *> IdentifierP *<
      token(LBRACE) *< token(DEF) *< token(MAIN) *< token(LPAREN) *< token(RPAREN) *<
      token(COLON) *< token(UNIT) *< token(EQSIGN) *< token(LBRACE) **
      star(StatementP) *< token(RBRACE) *< token(RBRACE) >> {
        case (id, stats) => MainObject(id, stats)
      }
    )

  val GoalP =
    positioned(
      MainObjectP ** star(ClassDeclP) *< token(EOF) >> {
        case (main, classes) => Program(main, classes)
      }
    )
}
