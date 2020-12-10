package elevator

import elevator.ElevatorControlSystem._
import elevator.ElevatorErr._
import izumi.distage.testkit.scalatest.DistageBIOEnvSpecScalatest
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{EitherValues, OptionValues}
import zio._

class ElevatorTest extends DistageBIOEnvSpecScalatest[ZIO] with OptionValues with EitherValues with TypeCheckedTripleEquals {
  "ElevatorControlSystem" must {
    "fail for more than 16 elevators" in {
      for {
        initialState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = 0.until(17).map { id =>
              id -> ElevatorState(id, 0, Up, Set.empty)
            }.toMap)
        }
        res <- ElevatorControlSystem(initialState).either
        _ = assert(res.left.value.continue(asString) === "maxElevatorExceeded 17")
      } yield ()
    }
  }
}
