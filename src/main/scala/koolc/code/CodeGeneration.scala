package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {
  private type VarSlots = Map[Symbol, Int]

  def run(ctx: Context)(prog: Program): Unit = {

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => classFile(sourceName, ct, outDir)
    }

    // Now do the main method
    val mainFile = new ClassFile(prog.main.getSymbol.name, None)
    mainFile.setSourceFile(sourceName)
    mainFile.addDefaultConstructor
    main(mainFile.addMainMethod.codeHandler, prog.main.stats, prog.main.getSymbol)
    mainFile.writeToFile(outDir + prog.main.getSymbol.name + ".class")
  }

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  private def classFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
    val classFile = new ClassFile(ct.getSymbol.name, ct.parent.map(_.getSymbol.name))
    classFile.setSourceFile(sourceName)
    classFile.addDefaultConstructor

    ct.vars.foreach { vr =>
      classFile.addField(vr.tpe.getType.jvmType, vr.getSymbol.name)
    }

    ct.methods.foreach { mt =>
      val mts = mt.getSymbol
      method(
        classFile.addMethod(
          mt.retType.getType.jvmType,
          mts.name,
          mts.argList.map(_.getType.jvmType).reduceOption(_ + _).getOrElse("")
        ).codeHandler,
        mt
      )
    }

    classFile.writeToFile(dir + ct.getSymbol.name + ".class")
  }

  private def method(ch: CodeHandler, mt: MethodDecl): Unit = {
    val ms = mt.getSymbol

    val withThis: VarSlots = Map(ms.classSymbol -> 0)
    val withArgs = (withThis /: ms.argList.zipWithIndex) {
      case (vars, (arg, i)) => vars + (arg -> (i+1))
    }
    val withLocals = (withArgs /: ms.members.values) {
      case (vars, local) => vars + (local -> ch.getFreshVar)
    }

    withAfter(ch, statement(ch, ms.classSymbol, withLocals, Block(mt.stats)))
    withAfter(ch, expression(ch, ms.classSymbol, withLocals, mt.retExpr))

    val ret = mt.retType.getType match {
      case TInt       => IRETURN
      case TIntArray  => ARETURN
      case TBoolean   => IRETURN
      case TString    => ARETURN
      case TObject(_) => ARETURN
      case _ => ???
    }
    ch << ret

    ch.freeze
  }

  private def main(ch: CodeHandler, stmts: List[StatTree], cs: ClassSymbol): Unit = {
    withAfter(ch, statement(ch, cs, Map(), Block(stmts)))
    ch << RETURN

    ch.freeze
  }

  private def withAfter(ch: CodeHandler, func: String => Unit): Unit = {
    val after = ch.getFreshLabel("after")
    func(after)
    ch << Label(after)
  }

  private def chooseStore(value: Typed) = value.getType match {
    case TInt       => IStore.apply _
    case TIntArray  => AStore.apply _
    case TBoolean   => IStore.apply _
    case TString    => AStore.apply _
    case TObject(_) => AStore.apply _
    case _ => ???
  }

  private def chooseLoad(value: Typed) = value.getType match {
    case TInt       => ILoad.apply _
    case TIntArray  => ALoad.apply _
    case TBoolean   => ILoad.apply _
    case TString    => ALoad.apply _
    case TObject(_) => ALoad.apply _
    case _ => ???
  }

  private def statement( ch: CodeHandler,
                         cs: ClassSymbol,
                         vars: VarSlots,
                         stmt: StatTree
                       )(
                         after: String
                       ): Unit = {
    ch << LineNumber(stmt.line)
    stmt match {
      case Block(stmts) =>
        stmts.foreach(st => withAfter(ch, statement(ch, cs, vars, st)))
        ch << Goto(after)

      case If(expr, thn, els) =>
        val nTrue = ch.getFreshLabel("true")
        val nFalse = ch.getFreshLabel("false")
        branch(ch, cs, vars, expr)(nTrue, nFalse)
        ch << Label(nTrue)
        statement(ch, cs, vars, thn)(after)
        ch << Goto(after) << Label(nFalse)
        els.map(statement(ch, cs, vars, _)(after))

      case While(expr, stat) =>
        val test = ch.getFreshLabel("test")
        val body = ch.getFreshLabel("body")
        ch << Label(test)
        branch(ch, cs, vars, expr)(body, after)
        ch << Label(body)
        statement(ch, cs, vars, stat)(test)

      case Println(expr) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        withAfter(ch, expression(ch, cs, vars, expr))
        ch << InvokeVirtual(
          "java/io/PrintStream",
          "println",
          "(%s)V".format(expr.getType.jvmType)
        ) << Goto(after)

      case Assign(id, expr) =>
        if (vars.contains(id.getSymbol)) {
          withAfter(ch, expression(ch, cs, vars, expr))
          ch << chooseStore(expr)(vars(id.getSymbol))
        } else {
          ch << ArgLoad(0)
          withAfter(ch, expression(ch, cs, vars, expr))
          ch << PutField(cs.name, id.getSymbol.name, id.getType.jvmType)
        }
        ch << Goto(after)

      case ArrayAssign(id, index, expr) =>
        if (vars.contains(id.getSymbol)) {
          ch << ALoad(vars(id.getSymbol))
        } else {
          ch << ArgLoad(0)
          ch << GetField(cs.name, id.getSymbol.name, id.getType.jvmType)
        }
        withAfter(ch, expression(ch, cs, vars, index))
        withAfter(ch, expression(ch, cs, vars, expr))
        ch << IASTORE << Goto(after)
    }
  }

  private def expression( ch: CodeHandler,
                          cs: ClassSymbol,
                          vars: VarSlots,
                          expr: ExprTree
                        )(
                          after: String
                        ): Unit = {
    ch << LineNumber(expr.line)
    expr match {
      case StringLit(value) =>
        ch << Ldc(value) << Goto(after)

      case IntLit(value) =>
        ch << Ldc(value) << Goto(after)

      case Plus(lhs, rhs) => expr.getType match {
        case TInt =>
          val nRhs = ch.getFreshLabel("rhs")
          val nOp = ch.getFreshLabel("op")
          expression(ch, cs, vars, lhs)(nRhs)
          ch << Label(nRhs)
          expression(ch, cs, vars, rhs)(nOp)
          ch << Label(nOp) << IADD << Goto(after)

        case TString =>
          val nalhs = ch.getFreshLabel("afterLHS")
          val narhs = ch.getFreshLabel("afterRHS")
          ch << DefaultNew("java/lang/StringBuilder")
          expression(ch, cs, vars, lhs)(nalhs)
          ch << Label(nalhs) <<
            InvokeVirtual(
              "java/lang/StringBuilder",
              "append",
              "(%s)Ljava/lang/StringBuilder;".format(lhs.getType.jvmType)
            )
          expression(ch, cs, vars, rhs)(narhs)
          ch << Label(narhs) <<
            InvokeVirtual(
              "java/lang/StringBuilder",
              "append",
              "(%s)Ljava/lang/StringBuilder;".format(rhs.getType.jvmType)
            ) <<
            InvokeVirtual(
              "java/lang/StringBuilder",
              "toString",
              "()Ljava/lang/String;"
            ) <<
            Goto(after)

        case _ => ???
      }

      case Minus(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("rhs")
        val nOp = ch.getFreshLabel("op")
        expression(ch, cs, vars, lhs)(nRhs)
        ch << Label(nRhs)
        expression(ch, cs, vars, rhs)(nOp)
        ch << Label(nOp) << ISUB << Goto(after)

      case Times(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("rhs")
        val nOp = ch.getFreshLabel("op")
        expression(ch, cs, vars, lhs)(nRhs)
        ch << Label(nRhs)
        expression(ch, cs, vars, rhs)(nOp)
        ch << Label(nOp) << IMUL << Goto(after)

      case Div(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("rhs")
        val nOp = ch.getFreshLabel("op")
        expression(ch, cs, vars, lhs)(nRhs)
        ch << Label(nRhs)
        expression(ch, cs, vars, rhs)(nOp)
        ch << Label(nOp) << IDIV << Goto(after)

      case ArrayRead(arr, index) =>
        withAfter(ch, expression(ch, cs, vars, arr))
        withAfter(ch, expression(ch, cs, vars, index))
        ch << IALOAD << Goto(after)

      case ArrayLength(arr) =>
        withAfter(ch, expression(ch, cs, vars, arr))
        ch << ARRAYLENGTH << Goto(after)

      case MethodCall(obj, meth, args) =>
        withAfter(ch, expression(ch, cs, vars, obj))
        args.foreach(st => withAfter(ch, expression(ch, cs, vars, st)))
        val mts = meth.getSymbol
        ch << InvokeVirtual(
          obj.getType.toString,
          mts.name,
          mts.getType.jvmType
          ) <<
          Goto(after)

      case id@Identifier(_) =>
        if (vars.contains(id.getSymbol)) {
          ch << chooseLoad(id)(vars(id.getSymbol))
        } else {
          ch << ArgLoad(0)
          ch << GetField(cs.name, id.getSymbol.name, id.getType.jvmType)
        }
        ch << Goto(after)

      case This() =>
        // This only makes sense in a non-static method
        ch << ArgLoad(0) << Goto(after)

      case NewIntArray(size) =>
        val nOp = ch.getFreshLabel("op")
        expression(ch, cs, vars, size)(nOp)
        ch << Label(nOp) << NewArray(10) // T_INT == 10
        ch << Goto(after)

      case New(tpe) =>
        ch << DefaultNew(tpe.getSymbol.name) << Goto(after)

      case boolExpr =>
        val nTrue = ch.getFreshLabel("true")
        val nFalse = ch.getFreshLabel("false")
        branch(ch, cs, vars, boolExpr)(nTrue, nFalse)
        ch << Label(nTrue) << Ldc(1) << Goto(after)
        ch << Label(nFalse) << Ldc(0) << Goto(after)
    }
  }

  private def branch( ch: CodeHandler,
                      cs: ClassSymbol,
                      vars: VarSlots,
                      expr: ExprTree
                    )(
                      nThen: String,
                      nElse: String
                    ): Unit = {
    ch << LineNumber(expr.line)
    expr match {
      case True() =>
        ch << Goto(nThen)

      case False() =>
        ch << Goto(nElse)

      case Not(expr) =>
        branch(ch, cs, vars, expr)(nElse, nThen)

      case And(lhs, rhs) =>
        val nNext = ch.getFreshLabel("next")
        branch(ch, cs, vars, lhs)(nNext, nElse)
        ch << Label(nNext)
        branch(ch, cs, vars, rhs)(nThen, nElse)

      case Or(lhs, rhs) =>
        val nNext = ch.getFreshLabel("next")
        branch(ch, cs, vars, lhs)(nThen, nNext)
        ch << Label(nNext)
        branch(ch, cs, vars, rhs)(nThen, nElse)

      case LessThan(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("nRhs")
        val nComp = ch.getFreshLabel("nComp")
        expression(ch, cs, vars, lhs)(nRhs)
        ch << Label(nRhs)
        expression(ch, cs, vars, rhs)(nComp)
        ch << Label(nComp) << If_ICmpLt(nThen) << Goto(nElse)

      case Equals(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("nRhs")
        val nComp = ch.getFreshLabel("nComp")
        expression(ch, cs, vars, lhs)(nRhs)
        ch << Label(nRhs)
        expression(ch, cs, vars, rhs)(nComp)

        val comparison = lhs.getType match {
          case TInt => If_ICmpEq
          case TBoolean => If_ICmpEq
          case _ => If_ACmpEq
        }
        ch << Label(nComp) << comparison(nThen) << Goto(nElse)

      case other =>
        val nNext = ch.getFreshLabel("next")
        expression(ch, cs, vars, other)(nNext)
        ch << Label(nNext) << IfEq(nElse) << Goto(nThen)
    }
  }
}
