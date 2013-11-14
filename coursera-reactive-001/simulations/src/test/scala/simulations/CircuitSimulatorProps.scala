package simulations

import org.scalacheck.{ Properties, Prop, Gen, Arbitrary }
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

object CircuitSimulatorProps extends Properties("CircuitSimulator") {

  object сircuitSimulator extends CircuitSimulator {
    val InverterDelay = 1
    val AndGateDelay = 3
    val OrGateDelay = 5
  }

  import сircuitSimulator._

  property("andGate") = forAll { (in1: Wire, in2: Wire) =>
    val out = new Wire
    andGate(in1, in2, out)
    run
    (in1.getSignal, in2.getSignal) match {
      case (true, true) => out.getSignal == true
      case _ => out.getSignal == false
    }
  }
  property("orGate") = forAll { (in1: Wire, in2: Wire) =>
    val out = new Wire
    orGate(in1, in2, out)
    run
    (in1.getSignal, in2.getSignal) match {
      case (false, false) => out.getSignal == false
      case _ => out.getSignal == true
    }
  }
  property("orGate2") = forAll { (in1: Wire, in2: Wire) =>
    val out = new Wire
    orGate2(in1, in2, out)
    run
    (in1.getSignal, in2.getSignal) match {
      case (false, false) => out.getSignal == false
      case _ => out.getSignal == true
    }
  }
  property("demux") = forAll { (in: Wire, c: List[Wire], out: List[Wire]) =>

    demux(in, c, out)
    run
    if (c.isEmpty) {
      out.filter(_.getSignal != false).isEmpty
    } else {
      val rc = c.reverse.zipWithIndex
      val rout = out.takeRight(c.size).reverse.zipWithIndex
      val result = for {
        (w, i) <- rc
        (ow, ii) <- rout.find(_._2 == i)
      } yield {
        val out = new Wire
        andGate(in, w, out)
        run
        out.getSignal == ow.getSignal
      }
      result.isEmpty || !result.exists(_ == false)
    }

  }

  implicit lazy val arbWire: Arbitrary[Wire] = Arbitrary(for {
    signal <- arbitrary[Boolean]
    wire = new Wire
  } yield {
    wire.setSignal(signal)
    wire
  })

}
