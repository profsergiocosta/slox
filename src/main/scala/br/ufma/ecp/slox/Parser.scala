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
    if matchToken(TokenType.PRINT) then printStatement()
    else if matchToken(TokenType.LEFT_BRACE) then Stmt.Block(block())
    else if matchToken(TokenType.IF) then ifStatement()
    else if matchToken(TokenType.WHILE) then whileStatement()
    else if matchToken(TokenType.FOR) then forStatement()
    else expressionStatement()

  private def ifStatement(): Stmt =
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")
    
    val thenBranch = statement()
    val elseBranch =
      if matchToken(TokenType.ELSE) then Some(statement())
      else None

    Stmt.If(condition, thenBranch, elseBranch)


  private def whileStatement(): Stmt =
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement()
    Stmt.While(condition, body)

  private def forStatement(): Stmt =
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

    val initializer: Option[Stmt] =
      if matchToken(TokenType.SEMICOLON) then
        None
      else if matchToken(TokenType.VAR) then
        varDeclaration()
      else
        Some(expressionStatement())

    val condition: Expr =
      if !check(TokenType.SEMICOLON) then
        expression()
      else
        Expr.Literal(Some(true))

    consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

    val increment: Option[Expr] =
      if !check(TokenType.RIGHT_PAREN) then
        Some(expression())
      else
        None

    consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")

    var body = statement()

    increment.foreach { incr =>
      body = Stmt.Block(List(body, Stmt.Expression(incr)))
    }

    body = Stmt.While(condition, body)

    initializer match
      case Some(init) => Stmt.Block(List(init, body))
      case None       => body


  private def block () : List[Stmt] =
    def loop(acc: List[Stmt]): List[Stmt] =
      if isAtEnd || check(RIGHT_BRACE) then acc.reverse
      else
        val decl = declaration()
        loop(decl match
          case Some(stmt) => stmt :: acc
          case None       => acc // erro sintático: ignora esta declaração
        )

    val result = loop(Nil)
    consume(RIGHT_BRACE, "Expect '}' after block.");
    result

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
    val expr = or()

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


  private def or(): Expr =
    var expr = and()

    while matchToken(TokenType.OR) do
      val operator = previous()
      val right = and()
      expr = Expr.Binary(expr, operator, right)

    expr

  private def and(): Expr =
    var expr = equality()

    while matchToken(TokenType.AND) do
      val operator = previous()
      val right = equality()
      expr = Expr.Binary(expr, operator, right)

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
    else call()

  private def call(): Expr =
    var expr = primary()

    var continue = true
    while continue do
      if matchToken(TokenType.LEFT_PAREN) then
        expr = finishCall(expr)
      else
        continue = false

    expr


  private def finishCall(callee: Expr): Expr =
    val arguments = scala.collection.mutable.ListBuffer.empty[Expr]

    if !check(TokenType.RIGHT_PAREN) then {
      arguments += expression() // Consome o primeiro argumento

      while matchToken(TokenType.COMMA) do
        if arguments.size >= 255 then
          error(peek(), "Can't have more than 255 arguments.")
        arguments += expression()
    }
    val paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")
    Expr.Call(callee, paren, arguments.toList)

  

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
