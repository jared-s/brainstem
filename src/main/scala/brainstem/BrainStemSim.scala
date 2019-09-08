package brainstem

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object BrainStemSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new BrainStem){dut =>
      dut.clockDomain.forkStimulus(period = 2)

      for(idx <- 0 to 1000){
        dut.clockDomain.waitRisingEdge()
      //  if(dut.io.halt.toBoolean)
      //    return
      }
    }
  }
}
