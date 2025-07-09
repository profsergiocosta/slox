package br.ufma.ecp.slox

trait LoxCallable:
  def arity: Int
  def call(interpreter: Interpreter, arguments: List[Any]): Any