package brainstem

import spinal.core._
import spinal.lib.PriorityMux
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


//noinspection TypeAnnotation,FieldFromDelayedInit
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

  val bootcode = new ArrayBuffer[BigInt]

  // [-]>]
  bootcode.append(Op.JumpF)
  bootcode.append(Op.DataDec)
  bootcode.append(Op.JumpB)
  bootcode.append(Op.Inc)
  bootcode.append(Op.JumpB)

  val codeMemSize = bootcode.size

  val codeMemInit: immutable.IndexedSeq[UInt] = {
    for(i <- 0 until codeMemSize) yield {
      U(bootcode(i), 8 bits)
    }
  }

  val codeWidth = 3
  val dataWidth = 16
  val codeAddr = Reg(UInt(log2Up(codeMemSize) bits))

  val codeMem = new Mem(UInt(codeWidth bits), codeMemSize)
  codeMem.init(codeMemInit)

  val dataMemSize = 3 * 1024
  val dataMem = new ICERam(dataWidth, dataMemSize)

  val dataAddr = Reg(UInt(8 bits))
  val data = Reg(UInt(dataWidth bits))

  val dataWriteEnable = Reg(Bool)
  val flip = Reg(Bool)

  val led5State = Reg(UInt(5 bits))
  io.LED1 := led5State(0)
  io.LED2 := led5State(1)
  io.LED3 := led5State(2)
  io.LED4 := led5State(3)
  io.LED5 := led5State(4)

  val led2State = Reg(UInt(2 bits))
  io.LEDR_N := led2State(0)
  io.LEDG_N := led2State(1)

  val led3State = Reg(UInt(3 bits))

  io.LED_RED_N := led3State(0)
  io.LED_GRN_N := led3State(1)
  io.LED_BLU_N := led3State(2)

  when(flip) {
    led5State := data.resized
    led3State(1) := dataWriteEnable
  }

  flip := ~flip

  val codeReady = Reg(Bool)
  val dataReady = Reg(Bool)
  val dataWriteAck = Reg(Bool)

  val state = Reg(UInt(32 bits))
  var fsm = new StateMachine {
    val Cold = new State with EntryPoint

    Cold.onEntry {
      codeReady := True
      led2State := 0
      led3State := 0
      led5State := 0
      data := 0
      dataAddr := 0
      dataReady := True
      dataWriteAck := True
    }

    Cold.whenIsActive {
      dataAddr := dataAddr + 1
      goto(Cold)
    }
  }
  val dataOut = Reg(UInt(dataWidth bits))

  dataMem.io.wdata := dataOut
  dataMem.io.addr := dataAddr
  dataMem.io.we := dataWriteEnable
  data := dataMem.io.rdata

  val cores = ArrayBuffer[BFCore]()

  for (i <- 0 until 2) {
      val core = new BFCore(codeWidth = codeWidth, dataWidth = dataWidth, codeMemSize = codeMemSize, dataMemSize = dataMemSize, pcInit = 0, dpInit = i)
      cores.append(core)
    core.io.code := codeMem(core.io.codeAddr)
    core.io.codeReady := codeReady
    core.io.data := data
    core.io.dataReady := dataReady
    core.io.dataWriteAck := dataWriteAck

  }

  dataAddr := PriorityMux(cores.map(c => c.io.dataAddrValid), cores.map(c => c.io.dataAddr)).resized
  codeAddr := PriorityMux(cores.map(c => c.io.codeAddrValid), cores.map(c => c.io.codeAddr)).resized
  dataOut := PriorityMux(cores.map(c => c.io.dataAddrValid), cores.map(c => c.io.dataOut)).resized
  dataWriteEnable := PriorityMux(cores.map(c => c.io.dataWriteEnable), cores.map(c => c.io.dataWriteEnable)).resized

  state := (dataAddr.resized ^ codeAddr.resized ^ dataOut.resized).resized

}

object BrainStemVerilog {
  def main(args: Array[String]) {
    SpinalConfig()
      .addStandardMemBlackboxing(blackboxOnlyIfRequested)
      .generateVerilog(new BrainStem).printPruned()
  }
}

