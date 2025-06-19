package com.example



import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._


object Lox {

  var hadError: Boolean = false

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

  private def run(source: String): Unit = {
    //val scanner = new Scanner(source)
    //val tokens: List[Token] = scanner.scanTokens()
    // For now, just print the tokens.
    //tokens.foreach(println)
    println("ok")
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    Console.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

}
