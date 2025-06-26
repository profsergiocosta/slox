package br.ufma.ecp.slox


import scala.collection.mutable


class Environment(val enclosing: Option[Environment] = None):
  private val values = mutable.Map.empty[String, Any]

  def define(name: String, value: Any): Unit =
    values(name) = value

  def get(token: Token): Any =
    token match
      case IdentifierToken(name, _) =>
        values.get(name) match
          case Some(value) => value
          case None =>
            enclosing match
              case Some(parent) => parent.get(token)
              case None => throw RuntimeError(token, s"Undefined variable '$name'.")
      case _ =>
        throw RuntimeError(token, "Invalid token for variable access.")

  def assign(name: Token, value: Any): Unit =
    name match
      case IdentifierToken(idName, _) =>
        if values.contains(idName) then
          values(idName) = value
        else
          enclosing match
            case Some(parent) => parent.assign(name, value)
            case None => throw RuntimeError(name, s"Undefined variable '$idName'.")
      case _ =>
        throw RuntimeError(name, "Invalid assignment target.")
