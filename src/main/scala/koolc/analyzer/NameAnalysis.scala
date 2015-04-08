package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    // All methods here have heavy side-effects!
    val gs = initial(ctx, prog)
    attach(ctx, prog, gs)
    checkClasses(ctx, prog.classes.map(_.getSymbol))
    prog
  }

  def initial[S <: Symbol](ctx: Context, prog: Program): GlobalScope = {
    def map[S <: Symbol](ss: List[Symbolic[S]]): Map[String, S] = {
      (Map[String, S]() /: ss) {
        (m, sc) => {
          val s = sc.getSymbol
          if (m.contains(s.name)) {
            ctx.reporter.error("Duplicate declaration of %s.".format(s.name), sc.getSymbol)
            ctx.reporter.error("First declaration is here.", m(s.name))
            m
          } else {
            m + (s.name -> s)
          }
        }
      }
    }

    def propagate[S <: Symbol](t: Tree, parent: Option[Symbolic[S]]): Unit = {
      t match {
        case o@MainObject(id, stats) => {
          val s = o.setSymbol(new ClassSymbol(id.value)).getSymbol
          s.setPos(o)
          id.setSymbol(s)

          s.members = Map()
          s.methods = Map()
        }

        case c@ClassDecl(id, parent, vars, methods) => {
          val s = c.setSymbol(new ClassSymbol(id.value)).getSymbol
          s.setPos(c)
          id.setSymbol(s)

          vars.foreach(propagate(_, Some(c)))
          methods.foreach(propagate(_, Some(c)))

          s.members = map(vars)
          s.methods = map(methods)
        }

        case v@VarDecl(tpe, id) => {
          val s = v.setSymbol(new VariableSymbol(id.value)).getSymbol
          s.setPos(v)
          id.setSymbol(s)
        }

        case m@MethodDecl(retType, id, args, vars, stats, retExpr) => {
          parent.get.getSymbol match {
            case ps: ClassSymbol => 
              val s = m.setSymbol(new MethodSymbol(id.value, ps)).getSymbol
              s.setPos(m)
              id.setSymbol(s)

            case _ =>
          }
        }

        case _ =>
      }
    }

    val s = new GlobalScope()

    val Program(main, classes) = prog
    propagate(main, None)
    classes.foreach(propagate(_, None))

    s.mainClass = main.getSymbol
    s.classes = map(classes)
    s
  }

  def attach(ctx: Context, prog: Program, gs: GlobalScope): Unit = {

  }

  def checkClasses(ctx: Context, cs: List[ClassSymbol]): Unit = {

  }
}
