package koolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
    def jvmType(): String
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def jvmType(): String = "V"
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def jvmType(): String = "V"
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def jvmType(): String = "I"
    override def toString = "Int"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def jvmType(): String = "[I"
    override def toString = "Int[]"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def jvmType(): String = "Z"
    override def toString = "Boolean"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def jvmType(): String = "Ljava/lang/String;"
    override def toString = "String"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      def subtype(parent: ClassSymbol, curr: ClassSymbol): Boolean =
        if (curr == parent) true
        else curr.parent.map(subtype(parent, _)).getOrElse(parent.getType == anyObject)

      tpe match {
        case TObject(inner) => subtype(inner, classSymbol)
        case _ => false
      }
    }
    override def jvmType(): String = "L%s;".format(classSymbol.name)
    override def toString = classSymbol.name
  }

  case class TMethod(methodSymbol: MethodSymbol, retType: Type) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == this
    override def jvmType(): String = "(%s)%s".format(
      methodSymbol.argList.map(_.getType.jvmType).mkString(""),
      retType.jvmType
    )
    override def toString = "(%s) => %s".format(
      methodSymbol.argList.map(_.getType).mkString(", "),
      retType
    )
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = {
    val cs = new ClassSymbol("Object")
    val ao = TObject(cs)
    cs.setType(ao)
    ao
  }
}
