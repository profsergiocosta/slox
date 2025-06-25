
package br.ufma.ecp.slox

class Interpreter:

  private val environment = Environment()

  def interpret(statements: List[Stmt]): Unit =
    try
      statements.foreach(execute)
    catch
      case err: RuntimeError => Lox.runtimeError(err)

  private def execute(stmt: Stmt): Unit =
    stmt match
      case Stmt.Expression(expr) => evaluate(expr)
      case Stmt.Print(expr) =>
        val value = evaluate(expr)
        println(stringify(value))
      case Stmt.Var(name, initializer) =>
        val value = initializer.map(evaluate).getOrElse(null)
        environment.define(name, value)

  private def evaluate(expr: Expr): Any =
    expr match
      case Expr.Assign(name, lrvalue) =>
        val value = evaluate(lrvalue)
        environment.assign(name, value)
        value
      case Expr.Literal(value) => value.getOrElse(null)
      case Expr.Grouping(e)    => evaluate(e)
      case Expr.Unary(op, right) =>
        val r = evaluate(right)
        op.tokenType match
          case TokenType.BANG  => !isTruthy(r)
          case TokenType.MINUS =>
            checkNumberOperand(op, r)
            -r.asInstanceOf[Double]
          case _ => throw RuntimeError(op, "Invalid unary operator.")
      case Expr.Binary(left, op, right) =>
        evaluateBinary(left, op, right)
      case Expr.Variable(name) =>
        environment.get(name)


  private def evaluateBinary(left: Expr, op: Token, right: Expr): Any =
        val l = evaluate(left)
        val r = evaluate(right)

        def numOp(f: (Double, Double) => Any): Any =
            (l, r) match
            case (lNum: Double, rNum: Double) => f(lNum, rNum)
            case _ => throw RuntimeError(op, "Operands must be numbers.")

        def addOp: Any =
            (l, r) match
            case (lNum: Double, rNum: Double) => lNum + rNum
            case (lStr: String, rStr: String) => lStr + rStr
            case _ => throw RuntimeError(op, "Operands must be two numbers or two strings.")

        op.tokenType match
            case TokenType.GREATER        => numOp(_ > _)
            case TokenType.GREATER_EQUAL  => numOp(_ >= _)
            case TokenType.LESS           => numOp(_ < _)
            case TokenType.LESS_EQUAL     => numOp(_ <= _)
            case TokenType.MINUS          => numOp(_ - _)
            case TokenType.SLASH          => numOp(_ / _)
            case TokenType.STAR           => numOp(_ * _)
            case TokenType.PLUS           => addOp
            case TokenType.BANG_EQUAL     => !isEqual(l, r)
            case TokenType.EQUAL_EQUAL    => isEqual(l, r)
            case _                        => throw RuntimeError(op, "Invalid binary operator.")


  private def isTruthy(value: Any): Boolean =
    value match
      case null       => false
      case b: Boolean => b
      case _          => true

  private def isEqual(a: Any, b: Any): Boolean =
    (a, b) match
      case (null, null) => true
      case (null, _)    => false
      case _            => a == b

  private def stringify(value: Any): String =
    value match
      case null      => "nil"
      case d: Double =>
        val str = d.toString
        if str.endsWith(".0") then str.dropRight(2) else str
      case _ => value.toString

  private def checkNumberOperand(operator: Token, operand: Any): Unit =
    if !operand.isInstanceOf[Double] then
      throw RuntimeError(operator, "Operand must be a number.")

  private def checkNumberOperands(operator: Token, left: Any, right: Any): Unit =
    if !(left.isInstanceOf[Double] && right.isInstanceOf[Double]) then
      throw RuntimeError(operator, "Operands must be numbers.")
