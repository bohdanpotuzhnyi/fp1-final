package com.tkroman.kpi.y2022.l1
enum IntOpCode:
  case Add, Mul

enum IntExpr:
  case Lit(n: Int)
  case Op(opCode: IntOpCode, l: IntExpr, r: IntExpr)

import IntOpCode.*
import IntExpr.*

def fold[A](e: IntExpr, fOp: Op => A, fLit: Lit => A): A =
  e match
    case Lit(n) => fLit(Lit(n))
    case Op(opCode, l, r) => fOp(Op(opCode, l, r))

def eval(e: IntExpr): Int =
  def fLit(lit: Lit): Int =
    lit.n
  def fOp(op: Op): Int =
    op.opCode match
      case Add => eval(op.l) + eval(op.r)
      case Mul => eval(op.l) * eval(op.r)

  fold(e, fOp, fLit)

def size(e: IntExpr): Int =
  def fLit(lit: Lit): Int =
    1
  def fOp(op: Op): Int =
    size(op.l) + size(op.r)

  fold(e, fOp, fLit)

def toString(e: IntExpr): String =
  def fLit(lit: Lit): String =
    lit.n.toString
  def fOp(op: Op): String =
    op.opCode match
      case Add => "(" + toString(op.l) + " + " + toString(op.r) + ")"
      case Mul => toString(op.l) + " * " + toString(op.r)

  fold(e, fOp, fLit)

def map(e: IntExpr, f: Int => Int): IntExpr =
  def fLit(lit: Lit): IntExpr =
    Lit(f(lit.n))
  def fOp(op: Op): IntExpr =
    Op(op.opCode, map(op.l, f), map(op.r, f))
  fold(e, fOp, fLit)

def rpn(s: String): IntExpr = {
  val items = s.split(" ")
  items.foldLeft(Nil: List[IntExpr])(foldingFunction).head
}

def foldingFunction(stack: List[IntExpr], a: String): List[IntExpr] =
  stack match {
    case Nil => Lit(a.toInt) :: stack
    case List(_) => Lit(a.toInt) :: stack
    case x::y::ys => a match {
      case "*" => Op(Mul, x, y) :: ys
      case "+" => Op(Add, x, y) :: ys
      case s: String => Lit(s.toInt) :: stack
    }
}

@main def run() =
  println("Hello")

  println(
    toString(rpn("10 4 3 + 2 * +"))
  )

  println(
    eval(rpn("10 4 3 + 2 * +"))
  )


  println(
    eval(Op(Add, Lit(5), Lit(3)))
  )

  println(
    size(Op(Add, Op(Mul, Lit(2), Lit(5)), Lit(3)))
  )

  println(
    toString(Op(Add, Op(Mul, Lit(2), Lit(5)), Lit(3)))
  )

  val f = (n: Int) => n*n
  println(
    toString(map(Op(Add, Lit(5), Lit(3)), f))
  )

