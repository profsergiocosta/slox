
package br.ufma.ecp.slox

class Interpreter:

  val globals = Environment()
  private var environment: Environment = globals


   // Registra função nativa no ambiente global:
  globals.define("clock", new LoxCallable:
    override def arity: Int = 0

    override def call(interpreter: Interpreter, arguments: List[Any]): Any =
      System.currentTimeMillis().toDouble / 1000.0

    override def toString: String = "<native fn>"
  )

  def interpret(statements: List[Stmt]): Unit =
    try
      statements.foreach(execute)
    catch
      case err: RuntimeError => Lox.runtimeError(err)

  private def execute(stmt: Stmt): Unit =
    stmt match
      case Stmt.Block (statements) => executeBlock(statements,Environment(Some(environment)))
      case Stmt.Expression(expr) => evaluate(expr)
      case Stmt.Print(expr) =>
        val value = evaluate(expr)
        println(stringify(value))
      case Stmt.Var(name, initializer) =>
        val value = initializer.map(evaluate).getOrElse(null)
        environment.define(name, value)
      case Stmt.If(condition, thenBranch, elseBranch) =>
        if isTruthy(evaluate(condition)) then
          execute(thenBranch)
        else
          elseBranch.foreach(execute)
      case Stmt.While(condition, body) =>
        while isTruthy(evaluate(condition)) do
          execute(body)

  def executeBlock(statements: List[Stmt], environment: Environment): Unit =
    val previous = this.environment
    try
      this.environment = environment
      statements.foreach(execute)
    finally
      this.environment = previous

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
            r match
              case d: Double => -d
              case _ => throw RuntimeError(op, "Operand must be a number.")
          case _ => throw RuntimeError(op, "Invalid unary operator.")
          
      case Expr.Binary(left, op, right) =>
        if op.tokenType == TokenType.OR || op.tokenType == TokenType.AND then
          evaluateLogical(left, op, right)
        else
          evaluateBinary(left, op, right)

      
      case Expr.Variable(name) =>
        environment.get(name)

      case Expr.Call(calleeExpr, paren, arguments) =>
        val callee = evaluate(calleeExpr)
        val args = arguments.map(evaluate)

        callee match
          case fn: LoxCallable =>
            if args.size != fn.arity then
              throw RuntimeError(paren, s"Expected ${fn.arity} arguments but got ${args.size}.")
            fn.call(this, args)
          case _ =>
            throw RuntimeError(paren, "Can only call functions and classes.")


  private def evaluateLogical(left: Expr, operator: Token, right: Expr): Any =
    val leftVal = evaluate(left)
    operator.tokenType match
      case TokenType.OR =>
        if isTruthy(leftVal) then leftVal else evaluate(right)
      case TokenType.AND =>
        if !isTruthy(leftVal) then leftVal else evaluate(right)
      case _ =>
        throw RuntimeError(operator, "Invalid logical operator.")

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
            case TokenType.BANG_EQUAL     => l != r 
            case TokenType.EQUAL_EQUAL    => l == r
            case _                        => throw RuntimeError(op, "Invalid binary operator.")


  private def isTruthy(value: Any): Boolean =
    value match
      case null       => false
      case b: Boolean => b
      case _          => true


  private def stringify(value: Any): String =
    value match
      case null      => "nil"
      case d: Double =>
        val str = d.toString
        if str.endsWith(".0") then str.dropRight(2) else str
      case _ => value.toString

