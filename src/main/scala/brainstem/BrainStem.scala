package brainstem

import spinal.core._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.{PriorityMux, Timeout}

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
    for (i <- 0 until codeMemSize) yield {
      U(bootcode(i), 8 bits)
    }
  }

  val codeWidth = 3
  val dataWidth = 128
  val codeAddr = Reg(UInt(log2Up(codeMemSize) bits))

  val codeMem = new Mem(UInt(codeWidth bits), codeMemSize)
  codeMem.init(codeMemInit)

  val dataMemSize = 2
  val dataMem = new Mem(UInt(dataWidth bits), dataMemSize)

  val dataAddr = Reg(UInt(log2Up(dataMemSize) bits))
  val data = Reg(UInt(dataWidth bits))
  val dataOut = Reg(UInt(dataWidth bits))

  val dataWriteEnable = Reg(Bool)

  val led5State = Reg(UInt(5 bits))
  io.LED1 := led5State(0)
  io.LED2 := led5State(1)
  io.LED3 := led5State(2)
  io.LED4 := led5State(3)
  io.LED5 := led5State(4)

  val led2State = Reg(UInt(2 bits))
  io.LEDR_N := led2State(0)
  io.LEDG_N := led2State(1)


  val codeReady = Reg(Bool)
  val dataReady = Reg(Bool)
  val dataWriteAck = Reg(Bool)

  val state = Reg(UInt(32 bits))
  var fsm = new StateMachine {
    val Cold = new State with EntryPoint

    Cold.onEntry {
      codeReady := True
      led2State := 3
      led5State := 0
      data := 0
      dataAddr := 0
      dataReady := True
      dataWriteAck := False
      state := 0
    }

    Cold.whenIsActive {
      dataAddr := dataAddr + 1
      when(dataWriteEnable) {
        dataMem(dataAddr) := dataOut
        dataWriteAck := True
      } otherwise {
        data := dataMem(dataAddr)
        dataWriteAck := False
      }

      state := state ^ data.resized

      val t = Timeout(17 ms)
      when(t) {
        led2State(0) := dataWriteEnable
        led2State(1) := dataWriteAck
        led5State := state.resized
        t.clear()
      }

      goto(Cold)
    }
  }


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
    SpinalConfig(defaultClockDomainFrequency = FixedFrequency(12 MHz))
      .addStandardMemBlackboxing(blackboxOnlyIfRequested)
      .generateVerilog(new BrainStem).printPruned()
  }
}

