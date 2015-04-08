package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    // All methods here have heavy side-effects!
    val gs = initial(ctx, prog)
    ctx.reporter.terminateIfErrors
    attach(ctx, prog, gs)
    ctx.reporter.terminateIfErrors
    inheritance(ctx, prog, gs)
    prog
  }

  def initial[S <: Symbol](ctx: Context, prog: Program): GlobalScope = {
    def doubleDeclaration[T <: Symbol](a: T, b: T) {
      val tpe = a match {
        case _: ClassSymbol => "Class-"
        case _: MethodSymbol => "Method-"
        case _: VariableSymbol => "Variable-"
        case _ => ""
      }
      ctx.reporter.error("Duplicate %sdeclaration of %s.".format(tpe, a.name), b)
      ctx.reporter.error("First declaration is here.", a)
    }

    def map[S <: Symbol](ss: List[Symbolic[S]], taken: Map[String, S]): Map[String, S] = {
      (Map[String, S]() /: ss) {
        (m, sc) => {
          val s = sc.getSymbol
          if (m.contains(s.name) || taken.contains(s.name)) {
            doubleDeclaration(m.get(s.name).orElse(taken.get(s.name)).get, s)
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

          s.members = map(vars, Map())
          s.methods = map(methods, Map())
        }

        case m@MethodDecl(retType, id, args, vars, stats, retExpr) => {
          parent.get.getSymbol match {
            case ps: ClassSymbol =>
              val s = m.setSymbol(new MethodSymbol(id.value, ps)).getSymbol
              s.setPos(m)
              id.setSymbol(s)

            case _ =>
          }

          args.foreach(propagate(_, Some(m)))
          vars.foreach(propagate(_, Some(m)))

          val s = m.getSymbol
          s.params = map(args, Map())
          s.members = map(vars, s.params)
        }

        case v@VarDecl(tpe, id) => {
          val s = v.setSymbol(new VariableSymbol(id.value)).getSymbol
          s.setPos(v)
          id.setSymbol(s)
        }

        case f@Formal(tpe, id) => {
          val s = f.setSymbol(new VariableSymbol(id.value)).getSymbol
          s.setPos(f)
          id.setSymbol(s)
        }

        case _ =>
      }
    }

    val s = new GlobalScope()

    val Program(main, classes) = prog
    propagate(main, None)
    classes.foreach(propagate(_, None))

    s.mainClass = main.getSymbol
    s.classes = map(classes, Map())

    if (s.classes.contains(s.mainClass.name)) {
      doubleDeclaration(s.mainClass, s.classes(s.mainClass.name))
    }

    s
  }

  def attach(ctx: Context, prog: Program, gs: GlobalScope): Unit = {
    def propagate[S <: Symbol](t: Tree, parent: Option[Symbolic[S]]): Unit = {
      t match {
        case o@MainObject(id, stats) => {
          val main = new MethodSymbol("main", o.getSymbol)
          statements(main, stats)
        }

        case c@ClassDecl(id, par, vars, methods) => {
          par match {
            case Some(p) => c.getSymbol.parent = typeparameter(p)
            case _ =>
          }

          vars.foreach(propagate(_, Some(c)))
          methods.foreach(propagate(_, Some(c)))
        }

        case m@MethodDecl(retType, id, args, vars, stats, retExpr) => {

        }

        case v@VarDecl(tpe, id) => {
          typeparameter(tpe)
        }

        case f@Formal(tpe, id) => {
          typeparameter(tpe)
        }

        case _ =>
      }
    }

    def typeparameter(t: TypeTree): Option[ClassSymbol] = t match {
      case i@Identifier(value) => {
        if (gs.classes.contains(value)) {
          val s = gs.classes(value)
          i.setSymbol(s)
          Some(s)
        } else if (value == gs.mainClass.getSymbol.name) {
          ctx.reporter.error("The main class '%s' is not a valid type.".format(value), i)
          None
        } else {
          ctx.reporter.error("Unknown type %s.".format(value))
          None
        }
      }
      case _ => None
    }

    def statements(ms: MethodSymbol, stats: List[StatTree]) {

    }
  }

  def inheritance(ctx: Context, prog: Program, gs: GlobalScope) {
    def circular(start: ClassSymbol): Bool = {
      def circ(curr: ClassSymbol): Bool =
        if (curr.name == cls.name)
            true
        else curr.parent match {
          case None => false
          case Some(c) => circ(c)
        }

      start.parent match {
        case None => false
        case Some(c) => circ(c)
      }
    }

    gs.classes.values.find(circular) match {
      case Some(c) => {
        ctx.reporter.error("Class %s has a circular inheritance graph.".format(c.name), c)
      }
      case None =>
    }
  }
}
