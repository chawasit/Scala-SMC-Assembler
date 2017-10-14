package com.github.chawasit.smc.assembler

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.language.postfixOps

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val arguments = new ArgumentConfiguration(args.toSeq)

    try {
      val lines: List[String] = readFile(arguments.input())
      val machineCodes: List[Int] = Assembler(lines) run()

      writeFile(arguments.output(), machineCodes.mkString("\n"))
    } catch {
      case error: Exception =>
        println(error.getMessage)
        System.exit(1)
    }
  }


  private def writeFile(output: String, data: String): Unit = {
    val writer = new PrintWriter(new File(output))
    writer.write(data)
    writer.close()
  }

  private def readFile(path: String): List[String] =
    Source.fromFile(path)
      .getLines()
      .toList

}
