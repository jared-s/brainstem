package brainstem

import spinal.core._
import spinal.lib._

import scala.util.Random

class BrainStem extends Component {
  val io = new Bundle {
  }
}

object BrainStemVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new BrainStem)
  }
}

