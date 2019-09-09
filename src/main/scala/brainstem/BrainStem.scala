package brainstem

import spinal.core._

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
    val output = out Bool
    val data = out UInt (8 bits)
  }

  noIoPrefix()

  val code = new ArrayBuffer[BigInt]

  // [-]>]
  code.append(Op.JumpF)
  code.append(Op.DataDec)
  code.append(Op.JumpB)
  code.append(Op.Inc)
  code.append(Op.JumpB)

  val codeMemSize = code.size

  val codeMemInit: immutable.IndexedSeq[UInt] = {
    for(i <- 0 until codeMemSize) yield {
      U(code(i), 8 bits)
    }
  }

  val codeWidth = 3
  val dataWidth = 8
  val codeMem = new Mem(UInt(codeWidth bits), codeMemSize)
  codeMem.init(codeMemInit)

  val dataMemSize = 4
  val dataMem = new Mem(UInt(dataWidth bits), dataMemSize)

  val core = new BFCore(codeWidth = codeWidth, dataWidth = dataWidth, codeMemSize = codeMemSize, dataMemSize = dataMemSize)

  val state = Reg(UInt(32 bits))

  core.io.code := codeMem(core.io.codeAddr)
  core.io.codeReady := True

  core.io.data := dataMem(core.io.dataAddr)
  core.io.dataReady := True

  when(core.io.dataWriteEnable) {
    dataMem(core.io.dataAddr) := core.io.dataOut
    core.io.dataWriteAck := True
  } otherwise {
    core.io.dataWriteAck := False
  }

  state := (state ^ core.io.dataOut.resized)
  state(0) := state(0) ^ core.io.dataAddrValid ^ core.io.codeAddrValid

  io.output := state.xorR
  io.data := core.io.data
}

object BrainStemVerilog {
  def main(args: Array[String]) {
    SpinalConfig().generateVerilog(new BrainStem).printPruned()
  }
}

