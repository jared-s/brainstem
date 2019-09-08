package brainstem

import spinal.core._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

/* BF to instruction bitmap:
  < 0
  > 1
  + 2
  - 3
  [ 4
  ] 5
 */
object Op {
  val Dec = 0
  val Inc = 1
  val DataInc = 2
  val DataDec = 3
  val JumpF = 4
  val JumpB = 5
}

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
    val halt = out Bool
  }

  noIoPrefix()

  val ledState = Reg(UInt(10 bits))

  val code = new ArrayBuffer[BigInt]


  // [-]+++
  code.append(Op.JumpF)
  code.append(Op.DataDec)
  code.append(Op.JumpB)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)
  code.append(Op.DataInc)

  val codeMemSize = code.size

  val codeMemInit: immutable.IndexedSeq[UInt] = {
    for(i <- 0 until codeMemSize) yield {
      U(code(i), 8 bits)
    }
  }

  val codeMem = new Mem(UInt(3 bits), codeMemSize)
  codeMem.init(codeMemInit)

  val dataMemSize = 128
  val dataMem = new Mem(UInt(8 bits), dataMemSize)

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
    val SeekForward = new State
    val SeekBack = new State

    Cold.onEntry {
      dataMem(0) := 2
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
      when(op === Op.Inc || op === Op.Dec) {
        dpIncDec := op === Op.Inc
        dpIncEnable := True
      } otherwise {
        // Data Operation
        when( op === Op.DataInc || op === Op.DataDec) {
          dataIncDec := op === Op.DataInc
          dataIncEnable := True
        }
      }
    }

    val data = Reg(UInt(8 bits))

    Decode.onEntry {
      data := dataMem(dp)
    }

    Decode.whenIsActive {
      val jmpFwd = op === Op.JumpF
      val jmpBack = op === Op.JumpB
      val isZero = data === 0

      when (jmpFwd && isZero) {
        goto(SeekForward)
      } otherwise {
        when(jmpBack && !isZero) {
          goto(SeekBack)
        } otherwise {
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

    SeekForward.onEntry {
      op := codeMem(pc)
    }

    SeekForward.whenIsActive {
      when(op === Op.JumpB) {
        goto(Fetch)
      } otherwise {
        pc := pc + 1
        goto(SeekForward)
      }
    }

    SeekBack.onEntry {
      op := codeMem(pc-1)
    }

    SeekBack.whenIsActive {
      op := codeMem(pc-1)

      when(op === Op.JumpF) {
        goto(Fetch)
      } otherwise {
        pc := pc - 1
        goto(SeekBack)
      }
    }
  }

  io.halt := pc === 0xF

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

