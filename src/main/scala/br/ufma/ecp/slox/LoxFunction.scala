package br.ufma.ecp.slox


class LoxFunction(declaration: Stmt.Function, closure: Environment) extends LoxCallable:

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val environment = Environment(Some(closure))

    for (i <- declaration.params.indices)
      declaration.params(i) match
        case IdentifierToken(name, _) =>
          environment.define(name, arguments(i))
        case _ =>
          throw RuntimeError(declaration.params(i), "Expected identifier as parameter.")

    try
      interpreter.executeBlock(declaration.body, environment)
      null
    catch case Return(value) => value

  override def arity: Int = declaration.params.length

  override def toString: String = declaration.name match
    case IdentifierToken(name, _) => s"<fn $name>"
    case _ => "<fn>"
