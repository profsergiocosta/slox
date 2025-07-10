package br.ufma.ecp.slox

// ⚡️ Para fluxo de controle: return dentro de funções
case class Return(value: Any)
    extends RuntimeException(null, null, false, false)

// ⚡️ Para erros reais de execução: tipo, variável indefinida, etc.
case class RuntimeError(token: Token, override val getMessage: String)
    extends RuntimeException(getMessage)
