package br.ufma.ecp.slox


import scala.collection.mutable


class Environment:
  private val values = mutable.Map.empty[String, Any]

  def define(name: String, value: Any): Unit =
    values(name) = value

  def get(token: Token): Any =
    token match
      case IdentifierToken(name, _) =>
        values.get(name) match
          case Some(value) => value
          case None =>
            throw RuntimeError(token, s"Undefined variable '$name'.")
      case _ =>
        throw RuntimeError(token, "Invalid token for variable access.")

