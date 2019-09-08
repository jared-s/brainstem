package brainstem

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

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

  val fsm = new StateMachine {
    val Reset = new State with EntryPoint
    val Cold = new State

    Reset.onEntry {
      ledState := 0
    }

    Reset.whenIsActive {
        goto(Cold)
    }

    Cold.whenIsActive {
      ledState := ledState + 1
    }
  }
}

object BrainStemVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new BrainStem)
  }
}

