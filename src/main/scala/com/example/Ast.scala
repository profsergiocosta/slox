package com.example


sealed trait Expr:
  def accept[R](visitor: Expr.Visitor[R]): R

object Expr:

  trait Visitor[R]:
    def visitBinaryExpr(expr: Binary): R
    def visitGroupingExpr(expr: Grouping): R
    def visitLiteralExpr(expr: Literal): R
    def visitUnaryExpr(expr: Unary): R

  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): R = visitor.visitBinaryExpr(this)

  case class Grouping(expression: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): R = visitor.visitGroupingExpr(this)

  case class Literal(value: Option[Any]) extends Expr:
    override def accept[R](visitor: Visitor[R]): R = visitor.visitLiteralExpr(this)

  case class Unary(operator: Token, right: Expr) extends Expr:
    override def accept[R](visitor: Visitor[R]): R = visitor.visitUnaryExpr(this)
