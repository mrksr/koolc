package koolc
package ast

import Trees._
import analyzer.Symbols._
import analyzer.Types._

object Printer {
  def apply(t: Tree): String = autoindent(pretty((t, p) => p)(t))

  def ast(t: Tree): String = t.toString
  def annotated(t: Tree): String = autoindent(pretty((t, p) => withType(t, withSymbol(t, p)))(t))

  private def autoindent(program: String) = {
    val indentString = "  ";
    def ids(n: Int) = (indentString * n).toList

    val withNewLines =
      (List[Char]() /: program) {
        case (out, c) => c match {
          case '{' => '\n' :: c :: out
          case '}' => '\n' :: c :: out
          case ';' => '\n' :: c :: out
          case _   => c :: out
        }
      }.reverse

    ((0, List[Char]()) /: ('.' :: withNewLines).zip(withNewLines)) {
      case ((indent, out), (prev, char)) => (prev, char) match {
        case ('\n', '}') => (indent - 1, '}' :: ids(indent - 1) ::: out)
        case ('\n', '{') => (indent + 1, char :: ids(indent) ::: out)
        case ('\n', _)   => (indent, char :: ids(indent) ::: out)
        case (_, '{')    => (indent + 1, char :: out)
        case (_, '}')    => (indent - 1, char :: out)
        case (_, _)      => (indent, char :: out)
      }
    }._2.reverse.mkString
  }

  private def withSymbol(t: Tree, p: String) = t match {
    case s: Symbolic[_] =>
      if (Set('}', ';') contains p.last)
        p
      else
        "%s#%s".format(p, s.sym.map(_.id).getOrElse("??"))
    case _ => p
  }

  private def withType(t: Tree, p: String) = t match {
    case t: Typed =>
      "<%s^%s>".format(p, t.getType)
    case _ => p
  }

  def pretty(annotator: (Tree, String) => String)(t: Tree): String = {
    def list(ts: List[Tree], join: String) = ts.map(pretty(annotator)) match {
      case Nil => ""
      case ps => ps.reduce(_ + join + _)
    }

    def option(to: Option[Tree], prefix: String) = to match {
      case None => ""
      case Some(t) => "%s%s".format(prefix, pretty(annotator)(t))
    }

    def block(t: StatTree) = t match {
      case bl@Block(_) => bl
      case _ => Block(List(t)).setPos(t)
    }

    val p = t match {
      case Program(main, classes)                              => {
        "%s%s".format(
          pretty(annotator)(main),
          list(classes, ""))
      }
      case MainObject(id, stats)                               => {
        "object %s {def main(): Unit = {%s}}".format(
          pretty(annotator)(id),
          list(stats, ""))
      }
      case ClassDecl(id, parent, vars, methods)                => {
        "class %s%s {%s%s}".format(
          pretty(annotator)(id),
          option(parent, " extends "),
          list(vars, ""),
          list(methods, ""))
      }
      case VarDecl(tpe, id)                                    => {
        "var %s: %s;".format(
          pretty(annotator)(id),
          pretty(annotator)(tpe))
      }
      case Formal(tpe, id)                                     => {
        "%s: %s".format(
          pretty(annotator)(id),
          pretty(annotator)(tpe))
      }
      case MethodDecl(retType, id, args, vars, stats, retExpr) => {
        "def %s(%s): %s = {%s%sreturn %s;}".format(
          pretty(annotator)(id),
          list(args, ", "),
          pretty(annotator)(retType), list(vars, ""),
          list(stats, ""),
          pretty(annotator)(retExpr))
      }

      case IntArrayType()                                      => {
        "Int[]"
      }
      case IntType()                                           => {
        "Int"
      }
      case BooleanType()                                       => {
        "Boolean"
      }
      case StringType()                                        => {
        "String"
      }

      case Block(stats)                                        => {
        "{%s}".format(
          list(stats, ""))
      }
      case If(expr, thn, els)                                  => {
        "if (%s) %s%s".format(
          pretty(annotator)(expr),
          pretty(annotator)(block(thn)),
          option(els.map(block), "else "))
      }
      case While(expr, stat)                                   => {
        "while (%s) %s".format(
          pretty(annotator)(expr),
          pretty(annotator)(block(stat)))
      }
      case Println(expr)                                       => {
        "println(%s);".format(
          pretty(annotator)(expr))
      }
      case Assign(id, expr)                                    => {
        "%s = %s;".format(
          pretty(annotator)(id),
          pretty(annotator)(expr))
      }
      case ArrayAssign(id, index, expr)                        => {
        "%s[%s] = %s;".format(
          pretty(annotator)(id),
          pretty(annotator)(index),
          pretty(annotator)(expr))
      }

      case And(lhs, rhs)                                       => {
        "%s && %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case Or(lhs, rhs)                                        => {
        "%s || %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case Plus(lhs, rhs)                                      => {
        "%s + %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case Minus(lhs, rhs)                                     => {
        "%s - %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case Times(lhs, rhs)                                     => {
        "%s * %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case Div(lhs, rhs)                                       => {
        "%s / %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case LessThan(lhs, rhs)                                  => {
        "%s < %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case Equals(lhs, rhs)                                    => {
        "%s == %s".format(
          pretty(annotator)(lhs),
          pretty(annotator)(rhs))
      }
      case ArrayRead(arr, index)                               => {
        "%s[%s]".format(
          pretty(annotator)(arr),
          pretty(annotator)(index))
      }
      case ArrayLength(arr)                                    => {
        "%s.length".format(
          pretty(annotator)(arr))
      }
      case MethodCall(obj, meth, args)                         => {
        "%s.%s(%s)".format(
          pretty(annotator)(obj),
          pretty(annotator)(meth),
          list(args, ", "))
      }
      case IntLit(value)                                       => {
        value.toString
      }
      case StringLit(value)                                    => {
        "\"%s\"".format(value)
      }

      case True()                                              => {
        "true"
      }
      case False()                                             => {
        "false"
      }
      case i@Identifier(value)                                 => {
        value
      }

      case t@This()                                            => {
        "this"
      }
      case NewIntArray(size)                                   => {
        "new Int[%s]".format(
          pretty(annotator)(size))
      }
      case New(tpe)                                            => {
        "new %s()".format(
          pretty(annotator)(tpe))
      }
      case Not(expr)                                           => {
        "! %s".format(
          pretty(annotator)(expr))
      }
    }

    annotator(t, p)
  }
}
