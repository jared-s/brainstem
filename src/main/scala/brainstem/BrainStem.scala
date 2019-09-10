package brainstem

import spinal.core._
import spinal.lib.PriorityMux

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
    val output = out Bool
    val data = out UInt (8 bits)
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
  val dataWidth = 8
  val codeMem = new Mem(UInt(codeWidth bits), codeMemSize)
  codeMem.init(codeMemInit)

  val dataMemSize = 24
  val dataMem = new Mem(UInt(dataWidth bits), dataMemSize)

  val cores = ArrayBuffer[BFCore]()

  for (i <- 0 until 10) {
    val core = new BFCore(codeWidth = codeWidth, dataWidth = dataWidth, codeMemSize = codeMemSize, dataMemSize = dataMemSize, pcInit = 0, dpInit = i)
    cores.append(core)
  }

  val state = Reg(UInt(32 bits))

  val codeAddrValids = cores.map(b => b.io.codeAddrValid)
  val codeAddrs = cores.map(b => b.io.codeAddr)
  val dataAddrValids = cores.map(b => b.io.dataAddrValid)
  val dataAddrs = cores.map(b => b.io.dataAddr)

  val codeDataReadies = cores.map(b => b.io.codeReady)

  val codeAddr = PriorityMux(codeAddrValids, codeAddrs)
  val dataAddr = PriorityMux(dataAddrValids, dataAddrs)

  val code = Reg(UInt(codeWidth bits))
  val data = Reg(UInt(dataWidth bits))
  val dataWrite = Reg(Bool)

  code := codeMem(codeAddr)
  data := dataMem(dataAddr)

  for (c <- cores.indices) {
    val core = cores(c)
    core.io.code := code
    core.io.codeReady := core.io.codeAddr === codeAddr

    core.io.data := data
    core.io.dataReady := core.io.dataAddr === dataAddr

    when(core.io.dataWriteEnable) {
      data := core.io.dataOut
      dataWrite := True
      core.io.dataWriteAck := True
    } otherwise {
      core.io.dataWriteAck := False
    }
  }

  when(dataWrite) {
    dataWrite := False
    dataMem(dataAddr) := data
  }

  state := (state ^ cores(0).io.dataOut.resized ^ cores(0).io.dataOut.resized)
  state(0) := state(0) ^ cores(0).io.dataAddrValid ^ cores(0).io.codeAddrValid

  io.output := state.xorR
  io.data := state.resized


}

object BrainStemVerilog {
  def main(args: Array[String]) {
    SpinalConfig()
      .addStandardMemBlackboxing(blackboxAllWhatsYouCan)
      .generateVerilog(new BrainStem).printPruned()
  }
}

