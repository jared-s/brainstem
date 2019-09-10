package brainstem

import spinal.core._


class ICERam(wordWidth: Int, wordCount: Int) extends BlackBox {
  val io = new Bundle {
    val clk = in Bool
    val we = in Bool
    val wdata = in UInt (16 bits)
    val rdata = out UInt (16 bits)
    val addr = in UInt (8 bits)
  }

  noIoPrefix()
  mapCurrentClockDomain(clock = io.clk)
  addRTLPath("./mem.v")
}
