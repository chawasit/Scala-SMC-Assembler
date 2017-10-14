package com.github.chawasit.smc.assembler

import org.rogach.scallop._

class ArgumentConfiguration(arguments: Seq[String])
  extends ScallopConf(arguments) {

  val input: ScallopOption[String] = trailArg[String](descr = "path to an assembly file")
  val output: ScallopOption[String] = trailArg[String](descr = "path to save machine code",
    default = Option[String]("output.txt"), required = false)

  printedName = "Assembler"

  verify()

}
