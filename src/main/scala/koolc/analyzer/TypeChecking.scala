package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    classes(ctx)(prog)
    variables(ctx)(prog)
    methods(ctx)(prog)
    ctx.reporter.terminateIfErrors
    statements(ctx)(prog)
    ctx.reporter.terminateIfErrors
    prog
  }

  private def classes(ctx: Context)(t: Tree): Unit = t match {
    case Program(main, clss) =>
      classes(ctx)(main)
      clss.foreach(classes(ctx))

    case m@MainObject(_, _) =>
      m.getSymbol.setType(TUntyped)

    case c@ClassDecl(_, _, _, _) =>
      c.getSymbol.setType(TObject(c.getSymbol))

    case _ =>
  }

  private def variables(ctx: Context)(t: Tree): Unit = {
    def single(sym: Symbol, tpeTree: TypeTree): Type = {
      val tpe: Type = tpeTree match {
        case IntType() => TInt
        case IntArrayType() => TIntArray
        case BooleanType() => TBoolean
        case StringType() => TString
        case i@Identifier(_) => i.getSymbol.getType
      }

      tpeTree.setType(tpe)
      sym.setType(tpe)
      tpe
    }

    t match {
      case Program(_, classes) =>
        classes.foreach(variables(ctx))

      case ClassDecl(_, _, vars, methods) =>
        vars.foreach(variables(ctx))
        methods.foreach(variables(ctx))

      case v@VarDecl(tpe, _) =>
        single(v.getSymbol, tpe)

      case f@Formal(tpe, _) =>
        single(f.getSymbol, tpe)

      case m@MethodDecl(retType, _, args, vars, _, _) =>
        m.getSymbol.setType(TMethod(m.getSymbol, single(m.getSymbol, retType)))
        args.foreach(variables(ctx))
        vars.foreach(variables(ctx))

      case _ =>
    }
  }

  private def methods(ctx: Context)(prog: Program): Unit = {
    def single(m: MethodSymbol): Unit = m.overridden match {
      case None =>
      case Some(parent) => {
        if (m.getType != parent.getType) {
          ctx.reporter.error("Overridden method '%s' has different return Type.".format(m.name), m)
          ctx.reporter.error("First declaration is here.", parent)
        } else {
          m.argList.zip(parent.argList).find{ case (l, r) => l.getType != r.getType } match {
            case None =>
            case Some((l, r)) =>
              ctx.reporter.error("Overridden method '%s' has parameter '%s' with different Type.".format(m.name, l.name), l)
              ctx.reporter.error("First declaration is here.", r)
          }
        }
      }
    }

    prog.classes.foreach(_.getSymbol.methods.values.foreach(single))
  }

  private def statements(ctx: Context)(prog: Program): Unit = {
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        // Base Cases
        case True() => TBoolean
        case False() => TBoolean
        case IntLit(_) => TInt
        case StringLit(_) => TString

        case i@Identifier(_) => i.getSymbol.getType
        case t@This() => t.getSymbol.getType

        case NewIntArray(size) =>
          tcExpr(size, TInt)
          TIntArray
        case New(id) => id.getSymbol.getType

        // Boolean Operators
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean

        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean

        case Not(expr) =>
          tcExpr(expr, TBoolean)
          TBoolean

        // Strictly Integer Operators
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt

        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt

        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt

        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBoolean

        // Overloaded Operators
        case Plus(lhs, rhs) =>
          val lt = tcExpr(lhs, TInt, TBoolean, TString)
          val rt = tcExpr(rhs, TInt, TBoolean, TString)
          (lt, rt) match {
            case (TInt, TInt) => TInt
            case _ => TString
          }

        case e@Equals(lhs, rhs) =>
          val lt = tcExpr(lhs, anyObject, TInt, TIntArray, TString, TBoolean)
          val rt = tcExpr(rhs, anyObject, TInt, TIntArray, TString, TBoolean)
          if (lt != rt) {
            (lt, rt) match {
              case (TObject(_), TObject(_)) =>
              case _ =>
                ctx.reporter.error("Cannot compare %s to %s".format(lt, rt), e)
            }
          }
          TBoolean

        // "Object" operations
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          TInt

        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
          TInt

        case MethodCall(obj, i@Identifier(name), args) =>
          val TObject(cs) = tcExpr(obj, anyObject)
          cs.lookupMethod(name) match {
            case None =>
              ctx.reporter.error("Type '%s' has no method '%s'".format(cs.name, name), i)
              TError
            case Some(method) =>
              if (args.length != method.argList.length)
                ctx.reporter.error("Wrong number of arguments for method '%s' in '%s'.".format(name, cs.name), i)
              else
                args.zip(method.argList).foreach {
                  case (expr, vs) => tcExpr(expr, vs.getType)
                }

              i.setSymbol(method)
              val TMethod(_, retType) = method.getType
              retType
          }
      }


      expr.setType(tpe)
      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        ctx.reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) =>
        stats.foreach(tcStat)

      case If(expr, thn, els) =>
        tcExpr(expr, TBoolean)
        tcStat(thn)
        els.map(tcStat)

      case While(expr, stat) =>
        tcExpr(expr, TBoolean)
        tcStat(stat)

      case Println(expr) =>
        tcExpr(expr, TString, TInt, TBoolean)

      case Assign(id, expr) =>
        tcExpr(expr, id.getSymbol.getType)

      case ArrayAssign(id, index, expr) =>
        tcExpr(index, TInt)
        tcExpr(expr, TInt)
    }

    def tcMethod(method: MethodDecl): Unit = method match {
      case m@MethodDecl(_, _, _, _, stats, retExpr) =>
        val TMethod(_, retType) = m.getSymbol.getType
        tcExpr(retExpr, retType)
        stats.foreach(tcStat)
    }

    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(_.methods.foreach(tcMethod))
  }
}
