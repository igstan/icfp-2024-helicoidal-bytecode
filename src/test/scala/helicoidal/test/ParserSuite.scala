package helicoidal
package test

import helicoidal.Parser.parse

final class ParserSuite extends munit.FunSuite {
  test("number") {
    assertEquals(parse("I/6"), Expr.Num(1337))
  }

  test("string") {
    assertEquals(parse("SB%,,/}Q/2,$_"), Expr.Str("B%,,/}Q/2,$_"))
  }

  test("unary negation") {
    assertEquals(parse("U- I$"), Expr.Uni(UniOp.Neg, Expr.Num(3)))
  }

  test("unary string-to-int") {
    assertEquals(parse("U# S4%34"), Expr.Uni(UniOp.S2I, Expr.Str("4%34")))
  }

  test("unary int-to-string") {
    assertEquals(parse("U$ I4%34"), Expr.Uni(UniOp.I2S, Expr.Num(15818151)))
  }
}
