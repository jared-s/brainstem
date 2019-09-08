package brainstem

import spinal.core._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


//noinspection TypeAnnotation
class BrainStem extends Component {
  val io = new Bundle {
    val LEDR_N = out Bool
    val LEDG_N = out Bool
    val LED_RED_N = out Bool
    val LED_GRN_N = out Bool
    val LED_BLU_N = out Bool
    val LED1 = out Bool
    val LED2 = out Bool
    val LED3 = out Bool
    val LED4 = out Bool
    val LED5 = out Bool
  }

  noIoPrefix()

  val ledState = Reg(UInt(10 bits))

  val code = new ArrayBuffer[BigInt]

  code.append(1)
  code.append(1)
  code.append(1)
  code.append(1)
  code.append(1)
  code.append(3)
  code.append(3)
  code.append(3)
  code.append(3)
  code.append(0)
  code.append(3)
  code.append(3)
  code.append(3)
  code.append(3)
  code.append(2)
  code.append(2)
  code.append(2)

  val codeMemSize = code.size

  /* BF to instruction bitmap:
    < 0
    > 1
    + 2
    - 3
   */
  for (_ <- 0 until codeMemSize) {
    val op = Random.nextInt(4)
    code.append(op)
  }

  val codeMemInit: immutable.IndexedSeq[UInt] = {
    for(i <- 0 until codeMemSize) yield {
      U(code(i), 8 bits)
    }
  }

  val codeMem = new Mem(UInt(3 bits), codeMemSize)
  codeMem.init(codeMemInit)

  val dataMemSize = 128
  val dataMemInit: immutable.IndexedSeq[UInt] = {
    for(_ <- 0 until dataMemSize) yield {
      U(0, 8 bits)
    }
  }
  val dataMem = new Mem(UInt(8 bits), dataMemSize)
  dataMem.init(dataMemInit)

  val pc = Reg(UInt(log2Up(codeMemSize) bits))
  val dp = Reg(UInt(log2Up(dataMemSize) bits))

  val dpIncDec = Reg(Bool)
  val dpIncEnable = Reg(Bool)
  val dataIncDec = Reg(Bool)
  val dataIncEnable = Reg(Bool)
  val op = Reg(UInt(3 bits))

  val fsm = new StateMachine {
    val Cold = new State with EntryPoint
    val Fetch = new State
    val Decode = new State
    val DataWrite = new State

    Cold.onEntry {
      ledState := 0
      pc := 0
      dp := 0
    }

    Cold.whenIsActive {
      goto(Fetch)
    }

    Fetch.onEntry {
      op := codeMem(pc)

      dpIncDec := False
      dpIncEnable := False
      dataIncDec := False
      dataIncEnable := False
      pc := pc + 1
    }

    Fetch.whenIsActive {
       goto(Decode)
    }

    Decode.onEntry {
      // PC operation
      when(op === 0 || op === 1) {
        dpIncDec := op === 1
        dpIncEnable := True
      } otherwise {
        // Data Operation
        when( op === 2 || op === 3) {
          dataIncDec := op === 3
          dataIncEnable := True
        }
      }
    }

    val data = Reg(UInt(8 bits))

    Decode.onEntry {
      data := dataMem(dp)
    }

    Decode.whenIsActive {
      when(dataIncEnable) {
        goto(DataWrite)
      } otherwise {
        goto(Fetch)
      }

      when(dpIncEnable) {
        when(dpIncDec) {
          dp := dp + 1
        } otherwise {
          dp := dp - 1
        }
      }
    }

    DataWrite.onEntry {
      when(dataIncDec) {
        dataMem(dp) := data + 1
      } otherwise {
        dataMem(dp) := data - 1
      }
    }

    DataWrite.whenIsActive {
      goto(Fetch)
    }
  }

  io.LEDR_N := ledState(0)
  io.LEDG_N := ledState(1)
  io.LED_RED_N := ledState(2)
  io.LED_GRN_N := ledState(3)
  io.LED_BLU_N := ledState(4)
  io.LED1 := ledState(5)
  io.LED2 := ledState(6)
  io.LED3 := ledState(7)
  io.LED4 := ledState(8)
  io.LED5 := ledState(9)
}

object BrainStemVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new BrainStem).printPruned()
  }
}

