package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {
  def run(ctx: Context)(prog: Program): Program = {
    // All methods here have heavy side-effects!
    val (gs, vars, clss) = initial(ctx, prog)
    ctx.reporter.terminateIfErrors
    attach(ctx, prog, gs, vars, clss)
    ctx.reporter.terminateIfErrors
    inheritance(ctx, prog, gs)
    ctx.reporter.terminateIfErrors
    prog
  }

  var vars = Set[VariableSymbol]()
  def initial[S <: Symbol](ctx: Context, prog: Program) = {
    def doubleDeclaration[T <: Symbol](a: T, b: T) {
      val tpe = a match {
        case _: ClassSymbol => "Class-"
        case _: MethodSymbol => "Method-"
        case _: VariableSymbol => "Variable-"
        case _ => ""
      }
      ctx.reporter.error("Duplicate %sdeclaration of '%s'.".format(tpe, a.name), b)
      ctx.reporter.error("First declaration is here.", a)
    }

    def map[S <: Symbol](ss: List[Symbolic[S]], taken: Map[String, S]): Map[String, S] =
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

    def propagate[S <: Symbol](t: Tree, parent: Option[Symbolic[S]]): Unit = t match {
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
          case ps: ClassSymbol => {
            val s = m.setSymbol(new MethodSymbol(id.value, ps)).getSymbol
            s.setPos(m)
            id.setSymbol(s)
          }

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
        vars += s
      }

      case f@Formal(tpe, id) => {
        val s = f.setSymbol(new VariableSymbol(id.value)).getSymbol
        s.setPos(f)
        id.setSymbol(s)
        vars += s
      }

      case _ =>
    }

    val gs = new GlobalScope()
    val Program(main, classes) = prog

    propagate(main, None)
    classes.foreach(propagate(_, None))

    gs.mainClass = main.getSymbol
    gs.classes = map(classes, Map())

    if (gs.classes.contains(gs.mainClass.name)) {
      doubleDeclaration(gs.mainClass, gs.classes(gs.mainClass.name))
      gs.classes = gs.classes - gs.mainClass.name
    }

    val clss = gs.classes.values.toSet
    (gs, vars, clss)
  }

  def attach(ctx: Context, prog: Program, gs: GlobalScope,
    vars: Set[VariableSymbol], clss: Set[ClassSymbol]): Unit = {
    var uvars = vars
    var uclss = clss
    def propagate[S <: Symbol](t: Tree, parent: Option[Symbolic[S]]): Unit = {
      t match {
        case Program(main, classes) => {
          propagate(main, None)
          classes.foreach(propagate(_, None))
        }

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
          args.foreach(propagate(_, Some(m)))
          vars.foreach(propagate(_, Some(m)))

          typeparameter(retType)
          statements(m.getSymbol, stats)
          expression(m.getSymbol, retExpr)
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
          uclss -= s
          Some(s)
        } else if (value == gs.mainClass.name) {
          ctx.reporter.error("The main class '%s' is not a valid type.".format(value), i)
          None
        } else {
          ctx.reporter.error("Unknown type '%s'.".format(value), i)
          None
        }
      }
      case _ => None
    }

    def statements(ms: MethodSymbol, stats: List[StatTree]) {
      def statement(stat: StatTree): Unit = stat match {
        case Block(ss) => statements(ms, ss)
        case If(expr, thn, None) => expression(ms, expr); statement(thn)
        case If(expr, thn, Some(els)) => expression(ms, expr); statement(thn); statement(els)
        case While(expr, stat) => expression(ms, expr); statement(stat)
        case Println(expr) => expression(ms, expr)
        case Assign(id, expr) => variable(ms, id); expression(ms, expr)
        case ArrayAssign(id, index, expr) => variable(ms, id); expression(ms, index); expression(ms, expr)
      }

      stats.foreach(statement)
    }

    def expression(ms: MethodSymbol, expr: ExprTree): Unit = expr match {
      case And(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case Or(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case Plus(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case Minus(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case Times(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case Div(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case LessThan(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)
      case Equals(lhs, rhs) => expression(ms, lhs); expression(ms, rhs)

      case NewIntArray(size) => expression(ms, size)
      case Not(expr) => expression(ms, expr)
      case New(id) => typeparameter(id)

      case ArrayRead(arr, index) => expression(ms, arr); expression(ms, index)
      case ArrayLength(arr) => expression(ms, arr)
      case MethodCall(obj, meth, args) => expression(ms, obj); args.foreach(expression(ms, _))

      case i@Identifier(value) => variable(ms, i)
      case t@This() => t.setSymbol(ms.classSymbol); uclss -= ms.classSymbol

      case _ =>
    }

    def variable(ms: MethodSymbol, id: Identifier) {
      val Identifier(value) = id

      ms.lookupVar(value).orElse(ms.classSymbol.lookupVar(value)) match {
        case Some(s) => id.setSymbol(s); uvars -= s
        case None => ctx.reporter.error("Use of undeclared variable '%s'.".format(value), id)
      }
    }

    propagate(prog, None)

    uvars.foreach {
      v => ctx.reporter.warning("Variable '%s' defined but never used.".format(v.name), v)
    }
    uclss.foreach {
      c => ctx.reporter.warning("Class '%s' defined but never used.".format(c.name), c)
    }
  }

  def inheritance(ctx: Context, prog: Program, gs: GlobalScope) {
    def circular(start: ClassSymbol) = {
      def circ(curr: ClassSymbol): Boolean =
        if (curr.name == start.name)
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

    def shadowing(start: ClassSymbol) = {
      def field(vr: VariableSymbol, curr: ClassSymbol): Option[VariableSymbol] =
        curr.lookupVar(vr.name).orElse(curr.parent.map(field(vr, _)).flatten)

      def method(mt: MethodSymbol, curr: ClassSymbol): Option[MethodSymbol] =
        curr.lookupMethod(mt.name).orElse(curr.parent.map(method(mt, _)).flatten)

      start.parent match {
        case Some(p) => {
          start.members.values.map(m => field(m, p).map((_, m))).flatten.foreach {
            case (a, b) =>
              ctx.reporter.error("Disallowed shadowing of field '%s'.".format(a.name), b)
              ctx.reporter.error("First declaration is here.", a)
          }

          start.methods.values.map(m => method(m, p).map((_, m))).flatten.foreach {
            case (a, b) =>
              if (a.params.size == b.params.size) {
                b.overridden = Some(a)
              } else {
                ctx.reporter.error("Disallowed shadowing of method '%s'.".format(a.name), b)
                ctx.reporter.error("First declaration is here.", a)
              }
          }
        }
        case None =>
      }
    }

    gs.classes.values.find(circular) match {
      case Some(c) => {
        ctx.reporter.error("Class '%s' has a circular inheritance graph.".format(c.name), c)
      }
      case None =>
    }

    gs.classes.values.foreach(shadowing)
  }
}
