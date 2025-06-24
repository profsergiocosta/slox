package br.ufma.ecp.slox


sealed trait Expr

object Expr:
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: Option[Any]) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr


sealed trait Stmt

object Stmt:
  case class Expression(expr: Expr) extends Stmt
  case class Print(expr: Expr) extends Stmt