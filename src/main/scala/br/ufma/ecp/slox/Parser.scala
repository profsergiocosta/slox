package br.ufma.ecp.slox

import br.ufma.ecp.slox.TokenType.*

class Parser(tokens: List[Token]):

  private class ParseError extends RuntimeException

  private var current: Int = 0

  def parse(): Option[List[Stmt]] =
    def loop(acc: List[Stmt]): List[Stmt] =
      if isAtEnd then acc.reverse
      else
        val decl = declaration()
        loop(decl match
          case Some(stmt) => stmt :: acc
          case None       => acc // erro sintático: ignora esta declaração
        )

    val result = loop(Nil)
    if Lox.hadError then None else Some(result)



  private def declaration(): Option[Stmt] =
    try
      if matchToken(TokenType.VAR) then varDeclaration()
      else Some(statement())
    catch
      case _: ParseError =>
        synchronize()
        None

  private def varDeclaration(): Option[Stmt] =
    val nameToken = consume(TokenType.IDENTIFIER, "Expect variable name.").asInstanceOf[IdentifierToken]
    val name = nameToken.name
    val initializer =
      if matchToken(TokenType.EQUAL) then Some(expression()) else None
    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
    Some(Stmt.Var(name, initializer))

  private def statement(): Stmt =
    if matchToken(TokenType.PRINT) then
      printStatement()
    else
      expressionStatement()

  private def printStatement(): Stmt =
    val value = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value.")
    Stmt.Print(value)

  private def expressionStatement(): Stmt =
    val value = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    Stmt.Expression(value)

  private def expression(): Expr = assignment();

  private def assignment(): Expr =
    val expr = equality()

    if matchToken(TokenType.EQUAL) then
      val equals = previous()
      val value = assignment()

      expr match
        case Expr.Variable(name) =>
          Expr.Assign(name, value)
        case _ =>
          error(equals, "Invalid assignment target.")
          // retorna a própria expressão original após erro
          expr
    else
      expr


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
            case KeywordToken(TokenType.FALSE, _) =>
                advance()
                Expr.Literal(Some(false))

            case KeywordToken(TokenType.TRUE, _) =>
                advance()
                Expr.Literal(Some(true))

            case KeywordToken(TokenType.NIL, _) =>
                advance()
                Expr.Literal(None)

            case NumberToken(value, _) =>
                advance()
                Expr.Literal(Some(value))

            case StringToken(value, _) =>
                advance()
                Expr.Literal(Some(value))

            case SymbolToken(TokenType.LEFT_PAREN, _) =>
                advance()
                val expr = expression()
                consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
                Expr.Grouping(expr)
            
            case id: IdentifierToken  =>
              advance()
              Expr.Variable(id)

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
