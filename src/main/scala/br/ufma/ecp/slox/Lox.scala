package br.ufma.ecp.slox



import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._




object Lox {

  private val interpreter = Interpreter()

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

    parser.parse() match
      case Some(statements) if !Lox.hadError =>
        interpreter.interpret(statements)
      case _ =>
        // Erro já foi reportado por Lox.error(), nada a fazer aqui
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
