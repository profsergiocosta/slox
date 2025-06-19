package com.example


object AstPrinter extends Expr.Visitor[String]:

  def print(expr: Expr): String =
    expr.accept(this)

  override def visitBinaryExpr(expr: Expr.Binary): String =
    parenthesize(expr.operator.toString(), expr.left, expr.right)

  override def visitGroupingExpr(expr: Expr.Grouping): String =
    parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): String =
    expr.value.map(_.toString).getOrElse("nil")

  override def visitUnaryExpr(expr: Expr.Unary): String =
    parenthesize(expr.operator.toString(), expr.right)

  private def parenthesize(name: String, exprs: Expr*): String =
    s"($name ${exprs.map(_.accept(this)).mkString(" ")})"

  @main def runAstPrinter(): Unit =
    import Expr.*
    val expression = Binary(
      Unary(
        SymbolToken(TokenType.MINUS, 1),
        Literal(Some(123))
      ),
      SymbolToken(TokenType.STAR,  1),
      Grouping(
        Literal(Some(45.67))
      )
    )

    println(print(expression))
