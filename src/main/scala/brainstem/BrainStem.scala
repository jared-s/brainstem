package brainstem

import java.util.concurrent.atomic.AtomicInteger

import spinal.core._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


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
  val memSize = 128

  /* BF to instruction bitmap:
    < 0
    > 1
   */
  for (_ <- 0 until memSize) {
    val b0 = Random.nextBoolean()
    if (b0) {
      code.append(0)
    } else {
      code.append(1)
    }
  }

  val memInit = {
    for(i <- 0 until memSize) yield {
      U(code(i), 8 bits)
    }
  }

  val mem = new Mem(UInt(8 bits), memSize)
  mem.init(memInit)

  val pc = Reg(UInt(log2Up(memSize) bits))
  val pcIncDec = Reg(Bool)
  val pcIncEnable = Reg(Bool)

  val op = Reg(UInt(8 bits))

  val fsm = new StateMachine {
    val Cold = new State with EntryPoint
    val Fetch = new State
    val Decode = new State

    Cold.onEntry {
      ledState := 0
      pc := 0
    }

    Cold.whenIsActive {
      goto(Fetch)
    }

    Fetch.onEntry {
      op := mem(pc)
      when(pcIncEnable) {
        when(pcIncDec) {
          pc := pc + 1
        } otherwise {
          pc := pc - 1
        }
      }
      pcIncDec := False
      pcIncEnable := False
    }

    Fetch.whenIsActive {
       goto(Decode)
    }

    Decode.onEntry {
      when(op === 0 || op === 1) {
        pcIncDec := op === 1
        pcIncEnable := True
      }
    }

    Decode.whenIsActive {
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

