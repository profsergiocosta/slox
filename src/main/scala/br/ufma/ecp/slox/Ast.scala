package br.ufma.ecp.slox


sealed trait Expr

object Expr:
  case class Assign(name: Token, value: Expr) extends Expr
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: Option[Any]) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
  case class Variable(name: Token) extends Expr


sealed trait Stmt

object Stmt:
  case class Expression(expr: Expr) extends Stmt
  case class Print(expr: Expr) extends Stmt
  case class Var(name: String, initializer: Option[Expr]) extends Stmt
  case class Block(statements:List[Stmt]) extends Stmt
  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt
  case class While(condition: Expr, body: Stmt) extends Stmt
