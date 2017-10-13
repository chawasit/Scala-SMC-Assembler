package com.github.chawasit.smc.assembler

import com.github.chawasit.smc.assembler.CustomException._

import scala.util.matching.Regex
import scala.language.postfixOps

case class Assembler(lines: List[String]) {

  def run(): List[Int] = {
    val lineWithIndexes = lines zipWithIndex
    val labelIndexes = findLabels(lineWithIndexes)
    val instructions = ignoreLabels(lineWithIndexes)

    translate(instructions, labelIndexes)
  }


  private def findLabels(lineWithIndexes: List[(String, Int)]): Map[String, Int] = {
    val maybeDuplicatedLabelIndexes = lineWithIndexes flatMap { case (line, index) =>
      line match {
        case LabeledInstructionPattern(label, _) => Some((label, index))
        case _ => None
      }
    }

    val duplicatedLabels = maybeDuplicatedLabelIndexes.groupBy(_._1).collect { case (x, List(_, _, _*)) => x }
    if (duplicatedLabels.nonEmpty)
      throw DuplicateLabelException(duplicatedLabels.toList)

    maybeDuplicatedLabelIndexes.toMap
  }

  private def ignoreLabels(lineWithIndexes: List[(String, Int)]): List[(String, Int)] =
    lineWithIndexes flatMap { case (line, index) =>
      line match {
        case LabeledInstructionPattern(_, instruction) => Some(instruction, index)
        case line: String => Some(line, index)
      }
    }

  // TODO: Refactor
  private def translate(instructions: List[(String, Int)], labelIndexes: Map[String, Int]): List[Int] =
    instructions map { case (instruction, line) =>
      instruction match {
        case RTypePattern(opcode, rs, rt, rd) =>
          try {
            val opcodeDecimal =
              opcode match {
                case "add" => 0
                case "nand" => 1
              }

            RTypeDecimal(opcodeDecimal
              , RegisterNumberToDecimal(rs)
              , RegisterNumberToDecimal(rt)
              , RegisterNumberToDecimal(rd))
          } catch {
            case _: Throwable => throw InvalidRegisterException(line, instruction)
          }
        case ITypePattern(opcode, rs, rt, offset) =>
          try {
            val opcodeDecimal = opcode match {
              case "lw" => 2
              case "sw" => 3
              case "beq" => 4
            }

            val memoryAddressOffset: Int = isSymbolicAddress(offset) match {
              case true => opcode match {
                case "beq" => labelIndexes(offset) - (line + 1)
                case _ => labelIndexes(offset)
              }
              case false => offset.toInt
            }

            if (isValidOffsetRange(memoryAddressOffset))
              throw InvalidOffsetRangeException(line, instruction)

            val sub16bitMemoryAddressOffset = memoryAddressOffset < 0 match {
              case true => (memoryAddressOffset << 16) >>> 16
              case false => memoryAddressOffset
            }

            ITypeDecimal(opcodeDecimal
              , RegisterNumberToDecimal(rs)
              , RegisterNumberToDecimal(rt)
              , sub16bitMemoryAddressOffset)
          } catch {
            case _: IllegalArgumentException => throw InvalidRegisterException(line, instruction)
            case _: NoSuchElementException => throw UndefinedLabelException(line, instruction)
            case e: Exception => throw e
          }
        case JTypePattern(opcode, rs, rd) =>
          try {
            JTypeDecimal(5, RegisterNumberToDecimal(rs), RegisterNumberToDecimal(rd))
          } catch {
            case _: Throwable => throw InvalidRegisterException(line, instruction)
          }
        case OTypePattern(opcode) => opcode match {
          case "halt" => OTypeToDecimal(6)
          case "noop" => OTypeToDecimal(7)
        }
        case FTypePattern(opcode, value) =>
          if (isSymbolicAddress(value)) {
            labelIndexes(value)
          } else {
            value.toInt
          }
        case _ => throw UnknownOpcodeException(line, instruction)
      }
    }

  private def isSymbolicAddress(value: String): Boolean = !value.matches("""-?\d+""")

  private def isValidOffsetRange(memoryAddressOffset: Int): Boolean =
    memoryAddressOffset > 32767 || memoryAddressOffset < -32768

  private def RTypeDecimal(opcode: Int, rs: Int, rt: Int, rd: Int): Int =
    (opcode << 22) + (rs << 19) + (rt << 16) + rd

  private def ITypeDecimal(opcode: Int, rs: Int, rt: Int, offset: Int): Int =
    (opcode << 22) + (rs << 19) + (rt << 16) + offset

  private def JTypeDecimal(opcode: Int, rs: Int, rd: Int): Int =
    (opcode << 22) + (rs << 19) + (rd << 16)

  private def OTypeToDecimal(opcode: Int): Int =
    opcode << 22

  private def RegisterNumberToDecimal(numberString: String): Int = {
    val number = numberString.toInt
    if (number < 0 || number > 7)
      throw new IllegalArgumentException
    number
  }

  private lazy val LabeledInstructionPattern: Regex = """(\w+)([ \t]+.*)""".r
  private lazy val RTypePattern: Regex = """[ \t]+(add|nand)[ \t]+(\w+)[ \t]+(\w+)[ \t]+(\w+)""".r
  private lazy val ITypePattern: Regex = """[ \t]+(lw|sw|beq)[ \t]+(\w+)[ \t]+(\w+)[ \t]+(\w+)""".r
  private lazy val JTypePattern: Regex = """[ \t]+(jalr)[ \t]+(\w+)[ \t]+(\w+)""".r
  private lazy val OTypePattern: Regex = """[ \t]+(halt|noop)""".r
  private lazy val FTypePattern: Regex = """[ \t]+(\.fill)[ \t]+(-?\w+)""".r

}
