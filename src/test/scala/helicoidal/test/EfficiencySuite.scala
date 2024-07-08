package helicoidal
package test

import helicoidal.Eval.eval
import helicoidal.Parser.parse

final class EfficiencySuite extends munit.FunSuite {
  test("efficiency1") {
    // Original expression, using call-by-name function application:
    //
    // B$ L! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v!
    // B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! I" L!
    // B+ B+ v! v! B+ v! v!

    // Transformed to use strict function application.
    val src =
      """
      B! L! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! B! v! I" L! B+ B+ v! v! B+ v! v!
      """
    val expr = parse(src)
    println(eval(expr))
  }

  test("efficiency2") {
    val src =
      """
      B+ I7c B* B! B! L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L% ? B= v% I! I" B+ I" B! v$ B- v% I" I":c1+0 I!
      """.stripMargin
    val expr = parse(src)
    println(eval(expr))
  }
}
