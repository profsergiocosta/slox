package com.example


enum TokenType:
  // Single-character tokens.
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE
  case COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR

  // One or two character tokens.
  case BANG, BANG_EQUAL
  case EQUAL, EQUAL_EQUAL
  case GREATER, GREATER_EQUAL
  case LESS, LESS_EQUAL

  // Literals.
  case IDENTIFIER, STRING, NUMBER

  // Keywords.
  case AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR
  case PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE

  case EOF


sealed trait Token:
  def tokenType: TokenType
  def lexeme: String
  def line: Int

case class IdentifierToken(lexeme: String, line: Int) extends Token:
  val tokenType = TokenType.IDENTIFIER

case class StringToken(lexeme: String, value: String, line: Int) extends Token:
  val tokenType = TokenType.STRING
  override def toString: String = s"$tokenType $lexeme \"$value\""

case class NumberToken(lexeme: String, value: Double, line: Int) extends Token:
  val tokenType = TokenType.NUMBER
  override def toString: String = s"$tokenType $lexeme $value"

case class SymbolToken(tokenType: TokenType, lexeme: String, line: Int) extends Token:
  override def toString: String = s"$tokenType $lexeme"

case class KeywordToken(tokenType: TokenType, lexeme: String, line: Int) extends Token:
  override def toString: String = s"$tokenType $lexeme"

case class EofToken(line: Int) extends Token:
  val tokenType = TokenType.EOF
  val lexeme = ""
  override def toString: String = s"$tokenType"