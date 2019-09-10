package brainstem

import spinal.core._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

class BFCore(codeWidth: Int = 3, dataWidth: Int = 8, codeMemSize: Int = 8, dataMemSize: Int = 8, pcInit: Int = 0, dpInit: Int = 0) extends Component {
  val io = new Bundle {
    val codeAddr = out UInt (log2Up(codeMemSize) bits)
    val codeAddrValid = out Bool

    val code = in UInt (codeWidth bits)
    val codeReady = in Bool

    val dataAddr = out UInt (log2Up(dataMemSize) bits)
    val dataAddrValid = out Bool

    val data = in UInt (dataWidth bits)
    val dataReady = in Bool

    val dataOut = out UInt (dataWidth bits)
    val dataWriteEnable = out Bool
    val dataWriteAck = in Bool
  }

  val pc = Reg(UInt(log2Up(codeMemSize) bits))
  val dp = Reg(UInt(log2Up(dataMemSize) bits))

  io.codeAddr := pc
  io.dataAddr := dp

  val dpIncDec = Reg(Bool)
  val dpIncEnable = Reg(Bool)
  val dataIncDec = Reg(Bool)
  val dataIncEnable = Reg(Bool)
  val op = Reg(UInt(3 bits))

  val codeAddrValid = Reg(Bool) init False
  val dataAddrValid = Reg(Bool) init False
  val dataWriteEnable = Reg(Bool)
  val dataOut = Reg(UInt(dataWidth bits)) init 0

  io.codeAddrValid := codeAddrValid
  io.dataAddrValid := dataAddrValid
  io.dataWriteEnable := dataWriteEnable
  io.dataOut := dataOut

  val fsm = new StateMachine {
    val Cold = new State with EntryPoint
    val Fetch = new State
    val ReadData = new State
    val Decode = new State
    val DataWrite = new State
    val SeekForwardFetch = new State
    val SeekForwardStep = new State

    val SeekBackFetch = new State
    val SeekBackStep = new State

    val data = Reg(UInt(dataWidth bits))

    Cold.onEntry {
      pc := pcInit
      dp := dpInit
    }

    Cold.whenIsActive {
      goto(Fetch)
    }

    Fetch.onEntry {
      codeAddrValid := True

      dpIncDec := False
      dpIncEnable := False
      dataIncDec := False
      dataIncEnable := False
      dataWriteEnable := False
      pc := pc + 1
    }

    Fetch.whenIsActive {
      when(!io.codeReady) {
        goto(Fetch)
      } otherwise {
        op := io.code
        codeAddrValid := False
        goto(ReadData)
      }
    }

    ReadData.onEntry {
      dataAddrValid := True
    }

    ReadData.whenIsActive {
      when(!io.dataReady) {
        goto(ReadData)
      } otherwise {
        data := io.data
        dataAddrValid := False
        goto(Decode)
      }
    }

    Decode.onEntry {
      data := io.data

      // PC operation
      when(op === Op.Inc || op === Op.Dec) {
        dpIncDec := op === Op.Inc
        dpIncEnable := True
      } otherwise {
        // Data Operation
        when(op === Op.DataInc || op === Op.DataDec) {
          dataIncDec := op === Op.DataInc
          dataIncEnable := True
        }
      }
    }

    Decode.whenIsActive {
      val jmpFwd = op === Op.JumpF
      val jmpBack = op === Op.JumpB
      val isZero = data === 0

      when(jmpFwd && isZero) {
        goto(SeekForwardFetch)
      } otherwise {
        when(jmpBack && !isZero) {
          goto(SeekBackFetch)
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
        data := data + 1
      } otherwise {
        data := data - 1
      }
    }

    DataWrite.whenIsActive {
      dataOut := data
      dataWriteEnable := True

      when(io.dataWriteAck) {
        dataWriteEnable := False
        goto(Fetch)
      } otherwise {
        goto(DataWrite)
      }
    }

    SeekForwardFetch.onEntry {
      codeAddrValid := True
    }

    SeekForwardFetch.whenIsActive {
      when(!io.codeReady) {
        goto(SeekForwardFetch)
      } otherwise {
        op := io.code
        codeAddrValid := False
        goto(SeekForwardStep)
      }
    }

    SeekForwardStep.whenIsActive {
      when(op === Op.JumpB) {
        goto(Fetch)
      } otherwise {
        pc := pc + 1
        goto(SeekForwardFetch)
      }
    }

    SeekBackFetch.onEntry {
      codeAddrValid := True
    }

    SeekBackFetch.whenIsActive {
      when(!io.codeReady) {
        goto(SeekBackFetch)
      } otherwise {
        op := io.code
        codeAddrValid := False
        goto(SeekBackStep)
      }
    }


    SeekBackStep.whenIsActive {
      when(op === Op.JumpF) {
        goto(Fetch)
      } otherwise {
        pc := pc - 1
        goto(SeekBackFetch)
      }
    }
  }

}
