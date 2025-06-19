package com.example



import scala.collection.mutable.ListBuffer



class Scanner(source: String):

  private val tokens = ListBuffer.empty[Token]
  private var start = 0
  private var current = 0
  private var line = 1

  private val keywords: Map[String, TokenType] = Map(
    "and"    -> TokenType.AND,
    "class"  -> TokenType.CLASS,
    "else"   -> TokenType.ELSE,
    "false"  -> TokenType.FALSE,
    "for"    -> TokenType.FOR,
    "fun"    -> TokenType.FUN,
    "if"     -> TokenType.IF,
    "nil"    -> TokenType.NIL,
    "or"     -> TokenType.OR,
    "print"  -> TokenType.PRINT,
    "return" -> TokenType.RETURN,
    "super"  -> TokenType.SUPER,
    "this"   -> TokenType.THIS,
    "true"   -> TokenType.TRUE,
    "var"    -> TokenType.VAR,
    "while"  -> TokenType.WHILE
  )

  def scanTokens(): List[Token] =
    while !isAtEnd do
      start = current
      scanToken()
    tokens += EofToken(line)
    tokens.toList

  private def isAtEnd: Boolean = current >= source.length

  private def scanToken(): Unit =
    val c = advance()
    c match
        case '(' => tokens += SymbolToken(TokenType.LEFT_PAREN, line)
        case ')' => tokens += SymbolToken(TokenType.RIGHT_PAREN, line)
        case '{' => tokens += SymbolToken(TokenType.LEFT_BRACE, line)
        case '}' => tokens += SymbolToken(TokenType.RIGHT_BRACE, line)
        case ',' => tokens += SymbolToken(TokenType.COMMA, line)
        case '.' => tokens += SymbolToken(TokenType.DOT, line)
        case '-' => tokens += SymbolToken(TokenType.MINUS, line)
        case '+' => tokens += SymbolToken(TokenType.PLUS, line)
        case ';' => tokens += SymbolToken(TokenType.SEMICOLON, line)
        case '*' => tokens += SymbolToken(TokenType.STAR, line)

        case '!' =>
            val tpe = if matchChar('=') then TokenType.BANG_EQUAL else TokenType.BANG
            tokens += SymbolToken(tpe, line)

        case '=' =>
            val tpe = if matchChar('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL
            tokens += SymbolToken(tpe, line)

        case '<' =>
            val tpe = if matchChar('=') then TokenType.LESS_EQUAL else TokenType.LESS
            tokens += SymbolToken(tpe, line)

        case '>' =>
            val tpe = if matchChar('=') then TokenType.GREATER_EQUAL else TokenType.GREATER
            tokens += SymbolToken(tpe, line)

        case '/' =>
            if matchChar('/') then
                while peek != '\n' && !isAtEnd do advance()
            else
                tokens += SymbolToken(TokenType.SLASH, line)

        case ' ' | '\r' | '\t' => ()
        case '\n' => line += 1
        case '"'  => string()
        case ch if ch.isDigit => number()
        case ch if ch.isLetter || ch == '_' => identifier()
        case _ => Lox.error(line, "Unexpected character.")

  private def identifier(): Unit =
    while peek.isLetterOrDigit || peek == '_' do advance()
    val text = source.substring(start, current)
    val tokenType = keywords.getOrElse(text, TokenType.IDENTIFIER)
    tokenType match
      case TokenType.IDENTIFIER => tokens += IdentifierToken(text, line)
      case kw                   => tokens += KeywordToken(kw, line)

  private def number(): Unit =
    while peek.isDigit do advance()
    if peek == '.' && peekNext().isDigit then
      advance()
      while peek.isDigit do advance()
    val value = source.substring(start, current).toDouble
    tokens += NumberToken(value, line)

  private def string(): Unit =
    while peek != '"' && !isAtEnd do
      if peek == '\n' then line += 1
      advance()
    if isAtEnd then
      Lox.error(line, "Unterminated string.")
    else
      advance() // consume closing quote
      val value = source.substring(start + 1, current - 1)
      tokens += StringToken(value, line)

  private def matchChar(expected: Char): Boolean =
    if isAtEnd || source.charAt(current) != expected then false
    else
      current += 1
      true

  private def peek: Char =
    if isAtEnd then '\u0000' else source.charAt(current)

  private def peekNext(): Char =
    if current + 1 >= source.length then '\u0000' else source.charAt(current + 1)

  private def advance(): Char =
    val ch = source.charAt(current)
    current += 1
    ch
