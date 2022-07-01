package com.tkroman.kpi.y2022.l1

import munit.FunSuite
import IntExpr.*
import IntOpCode.*

class IntExprTest extends FunSuite {

  test("eval_add"){
    val expected = 7
    val actual = {
      eval(
        Op(Add, Lit(2), Lit(5))
      )
    }
    assertEquals(actual, expected)
  }

  test("eval_mul"){
    val expected = 10
    val actual = {
      eval(
        Op(Mul, Lit(2), Lit(5))
      )
    }
    assertEquals(actual, expected)
  }

  test("eval_mix"){
    val expected = 15
    val actual = {
      eval(
        Op(Mul, Op(Add, Lit(2), Lit(1)), Lit(5))
      )
    }
    assertEquals(actual, expected)
  }

  test("size"){
    val expected = 3
    val actual = {
      size(
        Op(Add, Op(Add, Lit(2), Lit(1)), Lit(5))
      )
    }
    assertEquals(actual, expected)
  }

  test("map"){
    val expected = 34
    val actual = {
      eval(
        map(Op(Add, Lit(3), Lit(5)), (n: Int) => n*n)
      )
    }
    assertEquals(actual, expected)
  }

  test("rpn"){
    val expected = 24
    val actual = {
      eval(
        rpn("10 4 3 + 2 * +")
      )
    }
    assertEquals(actual, expected)
  }
}
