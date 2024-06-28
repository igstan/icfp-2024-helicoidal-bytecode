package helicoidal

import scala.collection.immutable.LongMap

enum Val {
  case Num(n: BigInt)
  case Str(s: String)
  case Bool(b: Boolean)
  case Fun(v: Long, body: Expr, closure: Env)
}

final case class Lazy(body: Expr, closure: Env)

final case class Env(bindings: LongMap[Lazy]) {
  def get(k: Long): Lazy =
    bindings.getOrElse(k, sys.error(s"undefined variable with index: $k"))

  def put(k: Long, v: Lazy): Env =
    Env(bindings.updated(k, v))
}

object Env {
  val empty: Env = Env(LongMap.empty)
}

object Eval {
  def eval(expr: Expr, env: Env = Env.empty): Val =
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
              case Val.Str(s) => Val.Num(Num.decode(Str.encode(s)))
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
          case BinOp.Add => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Num(a + b))
          case BinOp.Sub => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Num(a - b))
          case BinOp.Mul => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Num(a * b))
          case BinOp.Div => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Num(a / b))
          case BinOp.Mod => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Num(a % b))
          case BinOp.Lt  => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Bool(a < b))
          case BinOp.Gt  => evalNum(op, eval(a, env), eval(b, env), (a, b) => Val.Bool(a > b))
          case BinOp.Or  => evalBool(eval(a, env), eval(b, env), _ || _)
          case BinOp.And => evalBool(eval(a, env), eval(b, env), _ && _)

          case BinOp.Eq =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Num(a), Val.Num(b))   => Val.Bool(a == b)
              case (Val.Str(a), Val.Str(b))   => Val.Bool(a == b)
              case (Val.Bool(a), Val.Bool(b)) => Val.Bool(a == b)
              case _ => sys.error(s"eq operands must have identical types: $a, $b")
            }

          case BinOp.Cat =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Str(a), Val.Str(b)) => Val.Str(a + b)
              case _                        => sys.error(s"both operands must be strings: $a, $b")
            }

          case BinOp.Take =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Num(n), Val.Str(s)) => Val.Str(s.take(n.toInt))
              case _                        => sys.error(s"illegal operands for take: $a, $b")
            }

          case BinOp.Drop =>
            (eval(a, env), eval(b, env)) match {
              case (Val.Num(n), Val.Str(s)) => Val.Str(s.drop(n.toInt))
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

  private def evalNum(
    o: BinOp,
    a: Val,
    b: Val,
    op: (BigInt, BigInt) => Val,
  ): Val =
    (a, b) match {
      case (Val.Num(a), Val.Num(b)) => op(a, b)
      case _                        => sys.error(s"both operands must be numbers: $o, $a, $b")
    }

  private def evalBool(a: Val, b: Val, op: (Boolean, Boolean) => Boolean): Val =
    (a, b) match {
      case (Val.Bool(a), Val.Bool(b)) => Val.Bool(op(a, b))
      case _                          => sys.error(s"both operands must be booleans: $a, $b")
    }
}
