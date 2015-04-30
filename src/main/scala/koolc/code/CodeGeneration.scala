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

    withAfter(s => statement(Block(mt.stats))(s)(ch, ms.classSymbol, withLocals))(ch)
    withAfter(e => expression(mt.retExpr)(e)(ch, ms.classSymbol, withLocals))(ch)

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
    withAfter(a => statement(Block(stmts))(a)(ch, cs, Map()))(ch)
    ch << RETURN

    ch.freeze
  }

  private def withAfter( func: String => Unit
                       )(
                         implicit
                         ch: CodeHandler
                       ): Unit = {
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

  private def statement( stmt: StatTree
                       )(
                         after: String
                       )(
                         implicit
                         ch: CodeHandler,
                         cs: ClassSymbol,
                         vars: VarSlots
                       ): Unit = {
    ch << LineNumber(stmt.line)
    stmt match {
      case Block(stmts) =>
        stmts.foreach(st => withAfter(statement(st)))
        ch << Goto(after)

      case If(expr, thn, els) =>
        val nTrue = ch.getFreshLabel("true")
        val nFalse = ch.getFreshLabel("false")
        branch(expr)(nTrue, nFalse)
        ch << Label(nTrue)
        statement(thn)(after)
        ch << Goto(after) << Label(nFalse)
        els.map(statement(_)(after))

      case While(expr, stat) =>
        val test = ch.getFreshLabel("test")
        val body = ch.getFreshLabel("body")
        ch << Label(test)
        branch(expr)(body, after)
        ch << Label(body)
        statement(stat)(test)

      case Println(expr) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        withAfter(expression(expr))
        ch << InvokeVirtual(
          "java/io/PrintStream",
          "println",
          "(%s)V".format(expr.getType.jvmType)
        ) << Goto(after)

      case Assign(id, expr) =>
        if (vars.contains(id.getSymbol)) {
          withAfter(expression(expr))
          ch << chooseStore(expr)(vars(id.getSymbol))
        } else {
          ch << ArgLoad(0)
          withAfter(expression(expr))
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
        withAfter(expression(index))
        withAfter(expression(expr))
        ch << IASTORE << Goto(after)
    }
  }

  private def expression( expr: ExprTree
                        )(
                          after: String
                        )(
                          implicit
                          ch: CodeHandler,
                          cs: ClassSymbol,
                          vars: VarSlots
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
          expression(lhs)(nRhs)
          ch << Label(nRhs)
          expression(rhs)(nOp)
          ch << Label(nOp) << IADD << Goto(after)

        case TString =>
          val nalhs = ch.getFreshLabel("afterLHS")
          val narhs = ch.getFreshLabel("afterRHS")
          ch << DefaultNew("java/lang/StringBuilder")
          expression(lhs)(nalhs)
          ch << Label(nalhs) <<
            InvokeVirtual(
              "java/lang/StringBuilder",
              "append",
              "(%s)Ljava/lang/StringBuilder;".format(lhs.getType.jvmType)
            )
          expression(rhs)(narhs)
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
        expression(lhs)(nRhs)
        ch << Label(nRhs)
        expression(rhs)(nOp)
        ch << Label(nOp) << ISUB << Goto(after)

      case Times(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("rhs")
        val nOp = ch.getFreshLabel("op")
        expression(lhs)(nRhs)
        ch << Label(nRhs)
        expression(rhs)(nOp)
        ch << Label(nOp) << IMUL << Goto(after)

      case Div(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("rhs")
        val nOp = ch.getFreshLabel("op")
        expression(lhs)(nRhs)
        ch << Label(nRhs)
        expression(rhs)(nOp)
        ch << Label(nOp) << IDIV << Goto(after)

      case ArrayRead(arr, index) =>
        withAfter(expression(arr))
        withAfter(expression(index))
        ch << IALOAD << Goto(after)

      case ArrayLength(arr) =>
        withAfter(expression(arr))
        ch << ARRAYLENGTH << Goto(after)

      case MethodCall(obj, meth, args) =>
        withAfter(expression(obj))
        args.foreach(st => withAfter(expression(st)))
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
        expression(size)(nOp)
        ch << Label(nOp) << NewArray(10) // T_INT == 10
        ch << Goto(after)

      case New(tpe) =>
        ch << DefaultNew(tpe.getSymbol.name) << Goto(after)

      case boolExpr =>
        val nTrue = ch.getFreshLabel("true")
        val nFalse = ch.getFreshLabel("false")
        branch(boolExpr)(nTrue, nFalse)
        ch << Label(nTrue) << Ldc(1) << Goto(after)
        ch << Label(nFalse) << Ldc(0) << Goto(after)
    }
  }

  private def branch( expr: ExprTree
                    )(
                      nThen: String,
                      nElse: String
                    )(
                      implicit
                      ch: CodeHandler,
                      cs: ClassSymbol,
                      vars: VarSlots
                    ): Unit = {
    ch << LineNumber(expr.line)
    expr match {
      case True() =>
        ch << Goto(nThen)

      case False() =>
        ch << Goto(nElse)

      case Not(expr) =>
        branch(expr)(nElse, nThen)

      case And(lhs, rhs) =>
        val nNext = ch.getFreshLabel("next")
        branch(lhs)(nNext, nElse)
        ch << Label(nNext)
        branch(rhs)(nThen, nElse)

      case Or(lhs, rhs) =>
        val nNext = ch.getFreshLabel("next")
        branch(lhs)(nThen, nNext)
        ch << Label(nNext)
        branch(rhs)(nThen, nElse)

      case LessThan(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("nRhs")
        val nComp = ch.getFreshLabel("nComp")
        expression(lhs)(nRhs)
        ch << Label(nRhs)
        expression(rhs)(nComp)
        ch << Label(nComp) << If_ICmpLt(nThen) << Goto(nElse)

      case Equals(lhs, rhs) =>
        val nRhs = ch.getFreshLabel("nRhs")
        val nComp = ch.getFreshLabel("nComp")
        expression(lhs)(nRhs)
        ch << Label(nRhs)
        expression(rhs)(nComp)

        val comparison = lhs.getType match {
          case TInt => If_ICmpEq
          case TBoolean => If_ICmpEq
          case _ => If_ACmpEq
        }
        ch << Label(nComp) << comparison(nThen) << Goto(nElse)

      case other =>
        val nNext = ch.getFreshLabel("next")
        expression(other)(nNext)
        ch << Label(nNext) << IfEq(nElse) << Goto(nThen)
    }
  }
}
