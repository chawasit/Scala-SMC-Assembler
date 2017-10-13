package com.github.chawasit.smc.assembler

object CustomException {

  case class UndefinedLabelException(line: Int, label: String)
    extends Exception(s"Undefined Label. line:$line $label")

  case class DuplicateLabelException(labels: List[String])
    extends Exception(s"Duplicate Label. $labels")

  case class UnknownOpcodeException(line: Int, instruction: String)
    extends Exception(s"Unknown OPCODE. line:$line $instruction")

  case class InvalidRegisterException(line: Int, instruction: String)
    extends Exception(s"Invalid Register. line:$line $instruction")

  case class InvalidOffsetRangeException(line: Int, instruction: String)
    extends Exception(s"Invalid Offset Range. line:$line $instruction")

}
