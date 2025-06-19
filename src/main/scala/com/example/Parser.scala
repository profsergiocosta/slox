package com.example

import com.example.TokenType.*

class Parser(tokens: List[Token]):

  private class ParseError extends RuntimeException

  private var current: Int = 0

  def parse(): Option[Expr] =
    try Some(expression())
    catch
      case _: ParseError => None

  private def expression(): Expr = equality()

  private def equality(): Expr =
    var expr = comparison()
    while matchToken(BANG_EQUAL, EQUAL_EQUAL) do
      val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def comparison(): Expr =
    var expr = term()
    while matchToken(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) do
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def term(): Expr =
    var expr = factor()
    while matchToken(MINUS, PLUS) do
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def factor(): Expr =
    var expr = unary()
    while matchToken(SLASH, STAR) do
      val operator = previous()
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def unary(): Expr =
    if matchToken(BANG, MINUS) then
      val operator = previous()
      val right = unary()
      Expr.Unary(operator, right)
    else primary()

  private def primary(): Expr =
    peek() match
        case KeywordToken(TokenType.FALSE, _, _) =>
            advance()
            Expr.Literal(Some(false))

        case KeywordToken(TokenType.TRUE, _, _) =>
            advance()
            Expr.Literal(Some(true))

        case KeywordToken(TokenType.NIL, _, _) =>
            advance()
            Expr.Literal(None)

        case NumberToken(_, value: Double, _) =>
            advance()
            Expr.Literal(Some(value))

        case StringToken(_, value: String, _) =>
            advance()
            Expr.Literal(Some(value))

        case SymbolToken(TokenType.LEFT_PAREN, _, _) =>
            advance()
            val expr = expression()
            consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
            Expr.Grouping(expr)

        case _ =>
            throw error(peek(), "Expect expression.")


  private def matchToken(types: TokenType*): Boolean =
    types.exists(t => check(t)) && {
      advance()
      true
    } || false

  private def consume(t: TokenType, message: String): Token =
    if check(t) then advance()
    else throw error(peek(), message)

  private def error(token: Token, message: String): ParseError =
    Lox.error(token, message)
    new ParseError

  private def synchronize(): Unit =
    advance()
    while !isAtEnd do
      if previous().tokenType == SEMICOLON then return
      peek().tokenType match
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => advance()

  private def check(t: TokenType): Boolean =
    if isAtEnd then false else peek().tokenType == t

  private def advance(): Token =
    if !isAtEnd then current += 1
    previous()

  private def isAtEnd: Boolean = peek().tokenType == EOF

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)
