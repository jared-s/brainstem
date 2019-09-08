package brainstem

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object BrainStemSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new BrainStem){dut =>
      dut.clockDomain.forkStimulus(period = 10)

      for(idx <- 0 to 99){
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}
