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
  def line: Int
case class IdentifierToken(name: String, line: Int) extends Token:
  val tokenType = TokenType.IDENTIFIER
  override def toString: String = s"$tokenType $name"

case class StringToken(value: String, line: Int) extends Token:
  val tokenType = TokenType.STRING
  override def toString: String = s"$tokenType \"$value\""

case class NumberToken(value: Double, line: Int) extends Token:
  val tokenType = TokenType.NUMBER
  override def toString: String = s"$tokenType $value"

case class SymbolToken(tokenType: TokenType, line: Int) extends Token:
  override def toString: String = s"$tokenType"

case class KeywordToken(tokenType: TokenType, line: Int) extends Token:
  override def toString: String = s"$tokenType"

case class EofToken(line: Int) extends Token:
  val tokenType = TokenType.EOF
  override def toString: String = s"$tokenType"
