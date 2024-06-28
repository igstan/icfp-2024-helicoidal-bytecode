package helicoidal

import scala.collection.immutable.IntMap

enum Val {
  case Num(n: Int)
  case Str(s: String)
  case Bool(b: Boolean)
  case Fun(v: Int, body: Expr, closure: Env)
}

final case class Lazy(body: Expr, closure: Env)

final case class Env(bindings: IntMap[Lazy]) {
  def get(k: Int): Lazy = {
    println(s"getting var: $k")
    bindings.getOrElse(k, sys.error(s"undefined variable with index: $k"))
  }

  def put(k: Int, v: Lazy): Env =
    Env(bindings.updated(k, v))
}

object Env {
  val empty: Env = Env(IntMap.empty)
}

object Eval {
  def eval(expr: Expr, env: Env = Env.empty): Val = {
    println(s"$expr")
    expr match {
      case Expr.Num(n)  => Val.Num(n)
      case Expr.Str(s)  => Val.Str(Str.decode(s))
      case Expr.Bool(b) => Val.Bool(b)
      case Expr.Uni(op, a) =>
        op match {
          case UniOp.Neg =>
            eval(a, env) match {
              case Val.Num(n) => Val.Num(-n)
              case other      => sys.error(s"$op expected number, got $other")
            }

          case UniOp.Not =>
            eval(a, env) match {
              case Val.Bool(b) => Val.Bool(!b)
              case other       => sys.error(s"$op expected boolean, got $other")
            }

          case UniOp.S2I =>
            eval(a, env) match {
              case Val.Str(s) => Val.Num(Num.decode(Str.encode(s).drop(1)))
              case other      => sys.error(s"$op expected string, got $other")
            }

          case UniOp.I2S =>
            eval(a, env) match {
              case Val.Num(n) => Val.Str(Str.decode(Num.encode(n)))
              case other      => sys.error(s"$op expected number, got $other")
            }
        }

      case Expr.Bin(op, a, b) =>
        op match {
          case BinOp.Add => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Num(a + b))
          case BinOp.Sub => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Num(a - b))
          case BinOp.Mul => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Num(a * b))
          case BinOp.Div => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Num(a / b))
          case BinOp.Mod => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Num(a % b))
          case BinOp.Lt  => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Bool(a < b))
          case BinOp.Gt  => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Bool(a > b))
          case BinOp.Eq  => evalNum(eval(a, env), eval(b, env), (a, b) => Val.Bool(a == b))
          case BinOp.Or  => evalBool(eval(a, env), eval(b, env), _ || _)
          case BinOp.And => evalBool(eval(a, env), eval(b, env), _ && _)

          case BinOp.Cat =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Str(a), Val.Str(b)) => Val.Str(a + b)
              case _                        => sys.error(s"both operands must be strings: $a, $b")
            }

          case BinOp.Take =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Num(n), Val.Str(s)) => Val.Str(s.take(n))
              case _                        => sys.error(s"illegal operands for take: $a, $b")
            }

          case BinOp.Drop =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Num(n), Val.Str(s)) => Val.Str(s.drop(n))
              case _                        => sys.error(s"illegal operands for drop: $a, $b")
            }

          case BinOp.App =>
            eval(a, env) match {
              // Lazy evaluation: we don't evaluate b right now.
              case Val.Fun(v, body, closure) => eval(body, closure.put(v, Lazy(b, env)))
              case other                     => sys.error(s"cannot apply: $other is not a function")
            }
        }

      // Lazy evaluation: it's only now that we evaluate an identifier
      case Expr.Var(v) =>
        env.get(v) match {
          case Lazy(expr, closure) => eval(expr, closure)
        }

      case Expr.Fun(v, body) => Val.Fun(v, body, env)

      case Expr.Ife(cond, whenT, whenF) =>
        eval(cond, env) match {
          case Val.Bool(b) if b => eval(whenT, env)
          case Val.Bool(b)      => eval(whenF, env)
          case other            => sys.error(s"if condition must be a bool: $other")
        }
    }
  }

  private def evalNum(a: Val, b: Val, op: (Int, Int) => Val): Val =
    (a, b) match {
      case (Val.Num(a), Val.Num(b)) => op(a, b)
      case _                        => sys.error(s"both operands must be numbers: $a, $b")
    }

  private def evalBool(a: Val, b: Val, op: (Boolean, Boolean) => Boolean): Val =
    (a, b) match {
      case (Val.Bool(a), Val.Bool(b)) => Val.Bool(op(a, b))
      case _                          => sys.error(s"both operands must be booleans: $a, $b")
    }
}
