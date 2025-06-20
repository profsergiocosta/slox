package br.ufma.ecp.slox



import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._


case class RuntimeError(token: Token, override val getMessage: String)
    extends RuntimeException(getMessage)

object Lox {

  private val interpreter = Interpreter()

  def printExpr(expr: Expr): String = expr match
    case Expr.Literal(Some(value)) => value.toString
    case Expr.Literal(None)        => "nil"
    case Expr.Grouping(e)          => s"(group ${printExpr(e)})"
    case Expr.Unary(op, right)     => s"(${op.toString()} ${printExpr(right)})"
    case Expr.Binary(left, op, right) =>
      s"(${op.toString()} ${printExpr(left)} ${printExpr(right)})"

  var hadError: Boolean = false
  var hadRuntimeError: Boolean = false

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: jlox [script]")
      sys.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  private def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))

    // Indicate an error in the exit code.
    if (hadError) sys.exit(65)
  }

  private def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    // REPL read evaluate print loop
    while (true) {
      print("> ")
      val line = reader.readLine()
      if (line == null) return
      run(line)
      hadError = false
    }
  }

  def run(source: String): Unit =
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()

    val parser = new Parser(tokens)
    val expressionOpt = parser.parse()

    // Para simular o 'hadError', vamos supor que você tenha um objeto Lox com essa flag
    if Lox.hadError then return

    expressionOpt match
      case Some(expression) =>
        //println(AstPrinter.print(expression))
        //println(printExpr(expression))
         interpreter.interpret(expression);
      case None =>
        // Parser retornou erro (None), não faz nada ou exibe erro
        ()

  
    
  def runtimeError(error: RuntimeError): Unit =
    System.err.println(s"${error.getMessage}\n[line ${error.token.line}]")
    hadRuntimeError = true

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, message: String): Unit =
    val location = token match
      case t if t.tokenType == TokenType.EOF => " at end"
      case t => s" at '${t}'"

    report(token.line, location, message)

  private def report(line: Int, where: String, message: String): Unit = {
    Console.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

}
