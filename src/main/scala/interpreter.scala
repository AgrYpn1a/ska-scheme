package object skascheme {

  import scala.collection._


  // Applied sequence of arguments
  type Apply= Seq[SExpr] => SExpr

  def err(s: String): SList = 
    SList(Seq(SSymbol("error"), SString(s)))

  def isErr(e: SExpr): Boolean = e match {
    case l: SList => l.value match {
      case x :: xs => x match {
        case s: SSymbol if s.value.equals("error") => true
        case _ => false
      }

      case _ => false
    }

    case _ => false
  }

  def ok: SList = SList(Seq(SSymbol("ok")))

  // Binds variables from expression to their values
  // based on given environment
  def subst(localEnv: Map[SSymbol, SExpr], e: SExpr): SExpr =
    e match {
      case s: SSymbol if localEnv contains s => localEnv(s)
      case l: SList => SList(l.value.map { subst(localEnv, _) })

      // TODO possibly handle errors...
      case _ => e 
    }

  class Interpreter(val env: mutable.Map[SSymbol, Apply]) {
    // var env = mutable.Map[SSymbol, Reduction]()

    def reduce(e: SExpr): SExpr = e match {
      case s: SSymbol => err(s"${s.toStr}: undefined.")

      // Non-reducable types
      case a: SAtom =>    e
      case l: SLogical => e
      case n: SNil =>     e
      
      // If SList, check first item to determine
      // whether this is a function call, or a plain
      // list.
      case sl: SList => sl.value.headOption match {
        case Some(a: SSymbol) if env contains a => 
          env(a)(sl.value.toList.tail)
        case Some(e: SExpr) => err(s"${e.toStr} is not a function.")
        case None => err("Should be handled by parser.")
      }
    }

    def main(args: Array[String]): Unit = {
      println("Hello world")
    }
  }

  object Interpreter {
    val default = new Interpreter(mutable.Map())
  }

  // Init all modules
  Core
  Conditionals
}
