import skascheme._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck._

// mutable.T where T is name of collection Map, List...
import scala.collection._

class InterpreterTest extends
FlatSpec
with GeneratorDrivenPropertyChecks
with Matchers {

  "`define`" should "expand environment with new function definition" in {
    val expr = SList(Seq(
      SSymbol("define"),
      SList(Seq(SSymbol("f"), SSymbol("x"))),
      SList(Seq(SSymbol("+"), SSymbol("x"), SNumber(1)))))

    assert (Interpreter.default.reduce(expr) == ok)
    assert (Interpreter.default.reduce(SList(Seq(SSymbol("f"), SNumber(1)))) == SNumber(2))
    assert (Interpreter.default.reduce(SList(Seq(SSymbol("f"), SNumber(0)))) == SNumber(1))
    assert (Interpreter.default.reduce(SList(Seq(SSymbol("f"), SNumber(-1)))) == SNumber(0))
  }

  "`define`" should "err on missing arguments" in {
    val expr = SList(Seq(SSymbol("define"))) 
    val expected = err("define: Bad syntax.")

    assert(Interpreter.default.reduce(expr) == expected)
  }

  "`define`" should "err on missing body" in {
    val expr = SList(Seq(SSymbol("define"), SList(Seq(SSymbol("f"))))) 
    val expected = 
      err("define: Bad syntax (no expressions for procedure body)")

    assert(Interpreter.default.reduce(expr) == expected)
  }

  "`quote`" should "return the exact SExpr given without eval." in {
    val qNum = SList(Seq(SSymbol("quote"), SNumber(1)))
    val qList = SList(Seq(SSymbol("quote"), 
      SList(Seq(SNumber(1), SSymbol("x")))))
    val qNil = SList(Seq(SSymbol("quote"), SNil()))

    assert(Interpreter.default.reduce(qNum) == SNumber(1))
    assert(Interpreter.default.reduce(qList) == 
      SList(Seq(SNumber(1), SSymbol("x"))))
    assert(Interpreter.default.reduce(qNil) == SNil())
  }

  "`let`" should "expand env with defined variables." in {
    val expr = SList(Seq(
      SSymbol("let"),
      SList(Seq(
        SList(Seq(SSymbol("x"), SNumber(5))))),
      SList(Seq(SSymbol("+"), SSymbol("x"), SNumber(1)))))
    
    assert(Interpreter.default.reduce(expr) == SNumber(6))
  }

  // Arithmetic operations
  "`+`" should "return sum of the given arguments" in {
    val sum = SList(Seq(
      SSymbol("+"), SNumber(3), SNumber(2)))
    val sumRes = SNumber(5)

    assert(Interpreter.default.reduce(sum) == sumRes)
  }

  "`+`" should "sum arbitrary number of arguments" in {
    val n = scala.util.Random.nextInt(100)
    val gen = Gen.sized { n =>
      for {
        i <- Gen.choose(-1000, 1000)
      } yield i
    }

    var sum = 0
    var seq = mutable.Seq[SAtom]()

    forAll(gen) {
      case x  => 
        sum += x
        seq = seq :+ SNumber(x)
    }

    seq = SSymbol("+") +: seq

    val listSum = SList(seq)

    assert(SNumber(sum) == Interpreter.default.reduce(listSum))
  }

  "`+`" should "error on undefined symbol" in {
    assert (true == false)
  }

  "`+`" should "error on invalid argument" in {
    assert (true == false)
  }

}

