package com.github.chawasit.smc.assembler

import com.github.chawasit.smc.assembler.CustomException._
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class AssemblerSpec extends FlatSpec with Matchers{

  it should "Run correctly" in {
    val lines = loadResource("basic.asm")
    val machineCodes = Assembler(lines).run()

    machineCodes shouldBe a [List[_]]
  }

  it should "throw DuplicateLabelException if 2 or more labels have the same name." in {
    val lines = loadResource("duplicated_label.asm")
    lazy val machineCodes = Assembler(lines).run()

    a [DuplicateLabelException] should be thrownBy machineCodes
  }

  it should "throw UndefinedLabelException if use the label that not defined" in {
    val lines = loadResource("undefined_label.asm")
    lazy val machineCodes = Assembler(lines).run()

    a [UndefinedLabelException] should be thrownBy machineCodes
  }

  it should "throw UnknownOpcodeException if use not implemented opcode" in {
    val lines = loadResource("unknown_opcode.asm")
    lazy val machineCodes = Assembler(lines).run()

    a [UnknownOpcodeException] should be thrownBy machineCodes
  }

  it should "throw InvalidRegisterException if use wrong register number" in {
    val lines = loadResource("invalid_register.asm")
    lazy val machineCodes = Assembler(lines).run()

    a [InvalidRegisterException] should be thrownBy machineCodes
  }

  it should "throw InvalidOffsetRangeException if use out of range offset" in {
    val lines = loadResource("invalid_offset.asm")
    lazy val machineCodes = Assembler(lines).run()

    a [InvalidOffsetRangeException] should be thrownBy machineCodes
  }


  private def loadResource(filename: String): List[String] =
    Source.fromResource(filename).getLines().toList

}
