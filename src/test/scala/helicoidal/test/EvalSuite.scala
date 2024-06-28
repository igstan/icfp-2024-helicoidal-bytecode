package helicoidal
package test

import helicoidal.Eval.eval
import helicoidal.Parser.parse

final class EvalSuite extends munit.FunSuite {
  test("number") {
    assertEquals(eval(parse("I/6")), Val.Num(1337))
  }

  test("string") {
    assertEquals(eval(parse("SB%,,/}Q/2,$_")), Val.Str("Hello World!"))
  }

  test("unary negation") {
    assertEquals(eval(parse("U- I$")), Val.Num(-3))
  }

  test("unary string-to-int") {
    assertEquals(eval(parse("U# S4%34")), Val.Num(15818151))
  }

  test("unary int-to-string") {
    assertEquals(eval(parse("U$ I4%34")), Val.Str("test"))
  }

  test("binary operators") {
    assertEquals(eval(parse("B+ I# I$")), Val.Num(5))
    assertEquals(eval(parse("B- I$ I#")), Val.Num(1))
    assertEquals(eval(parse("B* I$ I#")), Val.Num(6))
    assertEquals(eval(parse("B/ U- I( I#")), Val.Num(-3))
    assertEquals(eval(parse("B% U- I( I#")), Val.Num(-1))
    assertEquals(eval(parse("B< I$ I#")), Val.Bool(false))
    assertEquals(eval(parse("B> I$ I#")), Val.Bool(true))
    assertEquals(eval(parse("B= I$ I#")), Val.Bool(false))
    assertEquals(eval(parse("B| T F")), Val.Bool(true))
    assertEquals(eval(parse("B& T F")), Val.Bool(false))
    assertEquals(eval(parse("B. S4% S34")), Val.Str("test"))
    assertEquals(eval(parse("BT I$ S4%34")), Val.Str("tes"))
    assertEquals(eval(parse("BD I$ S4%34")), Val.Str("t"))
  }

  test("if") {
    assertEquals(eval(parse("? B> I# I$ S9%3 S./")), Val.Str("no"))
  }

  test("lambda") {
    val src = "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"
    assertEquals(eval(parse(src)), Val.Str("Hello World!"))
  }

  test("quux") {
    val src = """B$ L# B$ L" B+ v" v" B* I$ I# v8"""
    val expr = parse(src)
    assertEquals(eval(expr), Val.Num(12))
  }

  test("y-combinator + forking addition") {
    val src = """B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L" L# ? B= v# I! I" B$ L$ B+ B$ v" v$ B$ v" v$ B- v# I" I%"""
    val expr = parse(src)
    println(expr)
    assertEquals(eval(expr), Val.Num(16))
  }
}
