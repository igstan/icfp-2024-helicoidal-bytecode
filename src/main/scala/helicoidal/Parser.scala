package helicoidal

// (B$ (B$ (L# (L$ v#)) (B. SB%,,/ S}Q/2,$_) IK)

// ((\v2 -> \v3 -> v2) ("Hello" . " World!")) 42

enum UniOp(t: Char) extends Enum[UniOp] {
  val token: String = s"U$t"

  case Neg extends UniOp('-')
  case Not extends UniOp('!')
  case S2I extends UniOp('#')
  case I2S extends UniOp('$')
}

enum BinOp(t: Char) extends Enum[BinOp] {
  val token: String = s"B$t"

  case Add  extends BinOp('+')
  case Sub  extends BinOp('-')
  case Mul  extends BinOp('*')
  case Div  extends BinOp('/')
  case Mod  extends BinOp('%')
  case Lt   extends BinOp('<')
  case Gt   extends BinOp('>')
  case Eq   extends BinOp('=')
  case Or   extends BinOp('|')
  case And  extends BinOp('&')
  case Cat  extends BinOp('.')
  case Take extends BinOp('T')
  case Drop extends BinOp('D')
  case App  extends BinOp('$')
}

enum Expr {
  case Var(v: Long)
  case Num(n: BigInt)
  case Str(s: String)
  case Bool(b: Boolean)
  case Uni(op: UniOp, a: Expr)
  case Bin(op: BinOp, a: Expr, b: Expr)
  case Ife(cond: Expr, whenT: Expr, whenF: Expr)
  case Fun(v: Long, body: Expr)

  override def toString: String =
    this match {
      case Expr.Var(v) => s"v$v"
      case Expr.Num(n) => s"$n"
      case Expr.Str(s) => helicoidal.Str.decode(s)
      case Expr.Bool(b) => s"$b"
      case Expr.Uni(op, a) => s"(${op.token} $a)"
      case Expr.Bin(op, a, b) if op != BinOp.App => s"(${op.token} ($a) ($b))"
      case Expr.Bin(op, a, b) => s"($a $b)"
      case Expr.Ife(cond, whenT, whenF) => s"(if ($cond) ($whenT) ($whenF))"
      case Expr.Fun(v, body) => s"(fun (v$v) $body)"
    }
}

object Num {
  def decode(nr: String): BigInt =
    nr.reverse
      .zipWithIndex
      .foldRight(BigInt(0)) {
        case ((char, pos), result) =>
          result + BigInt(char.toInt - 33) * BigInt(94).pow(pos)
      }

  def encode(n: BigInt): String = {
    var div = n
    val res = new StringBuilder

    while (div > 0) {
      val rem = div % 94
      res += (rem + 33).toChar
      div = div / 94
    }

    res.result().reverse
  }
}

final class Parser(source: String) {
  private val tokens = source.split("\\s+")
  private val total = tokens.length
  private var i = 0

  private def next(): String =
    if i >= total
    then sys.error("unexpected end of token stream")
    else {
      val t = tokens(i)
      i += 1
      t
    }

  def expr(): Expr = {
    import BinOp.*
    import UniOp.*

    val token = next()

    token match {
      case Neg.token  => Expr.Uni(Neg, expr())
      case Not.token  => Expr.Uni(Not, expr())
      case S2I.token  => Expr.Uni(S2I, expr())
      case I2S.token  => Expr.Uni(I2S, expr())
      case Add.token  => Expr.Bin(Add, expr(), expr())
      case Sub.token  => Expr.Bin(Sub, expr(), expr())
      case Mul.token  => Expr.Bin(Mul, expr(), expr())
      case Div.token  => Expr.Bin(Div, expr(), expr())
      case Mod.token  => Expr.Bin(Mod, expr(), expr())
      case Lt.token   => Expr.Bin(Lt, expr(), expr())
      case Gt.token   => Expr.Bin(Gt, expr(), expr())
      case Eq.token   => Expr.Bin(Eq, expr(), expr())
      case Or.token   => Expr.Bin(Or, expr(), expr())
      case And.token  => Expr.Bin(And, expr(), expr())
      case Cat.token  => Expr.Bin(Cat, expr(), expr())
      case Take.token => Expr.Bin(Take, expr(), expr())
      case Drop.token => Expr.Bin(Drop, expr(), expr())
      case App.token  => Expr.Bin(App, expr(), expr())
      case _ =>
        token.charAt(0) match {
          case 'T' => Expr.Bool(true)
          case 'F' => Expr.Bool(false)
          case 'I' => Expr.Num(Num.decode(token.drop(1)))
          case 'S' => Expr.Str(token.drop(1))
          case '?' => Expr.Ife(expr(), expr(), expr())
          case 'L' => Expr.Fun(Num.decode(token.substring(1)).toLong, expr())
          case 'v' => Expr.Var(Num.decode(token.substring(1)).toLong)
          case _   => sys.error(s"unknown token: $token")
        }
    }
  }
}

object Parser {
  def parse(s: String): Expr = new Parser(s).expr()
}
