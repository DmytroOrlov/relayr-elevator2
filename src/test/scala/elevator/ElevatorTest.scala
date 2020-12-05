package elevator

import elevator.ElevatorErr._
import izumi.distage.testkit.scalatest.DistageBIOEnvSpecScalatest
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{EitherValues, OptionValues}
import zio._

class ElevatorTest extends DistageBIOEnvSpecScalatest[ZIO] with OptionValues with EitherValues with TypeCheckedTripleEquals {
  "ElevatorControlSystem" must {
    "fail for more than 16 elevators" in {
      for {
        res <- ElevatorControlSystem(Seq.fill(17)((0, 0, 0))).either
        _ = assert(res.left.value.continue(asString) === "maxElevatorExceeded 17")
      } yield ()
    }
  }
}
