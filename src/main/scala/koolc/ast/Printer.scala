package koolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = autoindent(pretty(t))

  def ast(t: Tree): String = t.toString

  def autoindent(program: String) = {
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

  def pretty(t: Tree): String = {
    def list(ts: List[Tree], join: String) = ts.map(pretty) match {
      case Nil => ""
      case ps => ps.reduce(_ + join + _)
    }

    def option(to: Option[Tree], prefix: String) = to match {
      case None => ""
      case Some(t) => "%s%s".format(prefix, pretty(t))
    }

    def block(t: StatTree) = t match {
      case bl@Block(_) => bl
      case _ => Block(List(t)).setPos(t)
    }

    val p =
    t match {
      case Program(main, classes)                              => {
        "%s%s".format(
          pretty(main),
          list(classes, ""))
      }
      case MainObject(id, stats)                               => {
        "object %s {def main(): Unit = {%s}}".format(
          pretty(id),
          list(stats, ""))
      }
      case ClassDecl(id, parent, vars, methods)                => {
        "class %s%s {%s%s}".format(
          pretty(id),
          option(parent, " extends "),
          list(vars, ""),
          list(methods, ""))
      }
      case VarDecl(tpe, id)                                    => {
        "var %s: %s;".format(
          pretty(id),
          pretty(tpe))
      }
      case Formal(tpe, id)                                     => {
        "%s: %s".format(
          pretty(id),
          pretty(tpe))
      }
      case MethodDecl(retType, id, args, vars, stats, retExpr) => {
        "def %s(%s): %s = {%s%sreturn %s;}".format(
          pretty(id),
          list(args, ", "),
          pretty(retType), list(vars, ""),
          list(stats, ""),
          pretty(retExpr))
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
          pretty(expr),
          pretty(block(thn)),
          option(els.map(block), "else "))
      }
      case While(expr, stat)                                   => {
        "while (%s) %s".format(
          pretty(expr),
          pretty(block(stat)))
      }
      case Println(expr)                                       => {
        "println(%s);".format(
          pretty(expr))
      }
      case Assign(id, expr)                                    => {
        "%s = %s;".format(
          pretty(id),
          pretty(expr))
      }
      case ArrayAssign(id, index, expr)                        => {
        "%s[%s] = %s;".format(
          pretty(id),
          pretty(index),
          pretty(expr))
      }

      case And(lhs, rhs)                                       => {
        "%s && %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case Or(lhs, rhs)                                        => {
        "%s || %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case Plus(lhs, rhs)                                      => {
        "%s + %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case Minus(lhs, rhs)                                     => {
        "%s - %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case Times(lhs, rhs)                                     => {
        "%s * %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case Div(lhs, rhs)                                       => {
        "%s / %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case LessThan(lhs, rhs)                                  => {
        "%s < %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case Equals(lhs, rhs)                                    => {
        "%s == %s".format(
          pretty(lhs),
          pretty(rhs))
      }
      case ArrayRead(arr, index)                               => {
        "%s[%s]".format(
          pretty(arr),
          pretty(index))
      }
      case ArrayLength(arr)                                    => {
        "%s.length".format(
          pretty(arr))
      }
      case MethodCall(obj, meth, args)                         => {
        "%s.%s(%s)".format(
          pretty(obj),
          pretty(meth),
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
        "%s#%s".format(value, i.sym.map(_.id).getOrElse("??"))
      }

      case This()                                              => {
        "this"
      }
      case NewIntArray(size)                                   => {
        "new Int[%s]".format(
          pretty(size))
      }
      case New(tpe)                                            => {
        "new %s()".format(
          pretty(tpe))
      }
      case Not(expr)                                           => {
        "! %s".format(
          pretty(expr))
      }
    }

    // "[%s]%s".format(t.position, p)
    p
  }
}
