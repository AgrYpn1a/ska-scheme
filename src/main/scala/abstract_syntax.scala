package skascheme {

  sealed trait SExpr {
    def toStr: String
  }

  sealed trait SLogical extends SExpr
  case object STrue    extends SLogical {
    override def toStr = "true"
  }
  case object SFalse   extends SLogical {
    override def toStr = "false"
  }

  case class SNil() extends SExpr {
    override def toStr = "null"
  }

  sealed trait SAtom extends SExpr

  // Atoms
  case class SNumber(value: Int)    extends SAtom {
    override def toStr: String = value + ""
  }

  case class SChar(value: Char)     extends SAtom {
    override def toStr: String = value + ""
  }

  case class SString(value: String) extends SAtom {
    override def toStr: String = value
  }

  case class SSymbol(value: String) extends SAtom {
    override def toStr: String = value
  }

  // Non-trivial expressions
  case class SList(value: Seq[SExpr]) extends SExpr {
    override def toStr: String = "list"
  }

}
