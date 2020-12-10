package elevator

import elevator.ElevatorErr._
import elevator.fixtures.Rnd.rnd
import izumi.distage.testkit.scalatest.DistageBIOEnvSpecScalatest
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{EitherValues, OptionValues}
import zio._

class ElevatorsTest extends DistageBIOEnvSpecScalatest[ZIO] with OptionValues with EitherValues with TypeCheckedTripleEquals {
  "ElevatorControlSystem" must {
    "fail for more than 16 elevators" in {
      for {
        initialState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = 0.until(17).map { id =>
              id -> ElevatorState(id, 0, Up, Set.empty)
            }.toMap,
          )
        }
        res <- ElevatorControlSystem(initialState).either
        _ = assert(res.left.value.continue(asString) === "wrongElevatorNumber: 17 should be 1..16")
      } yield ()
    }
    "fail for 0 elevators" in {
      for {
        initialState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map.empty,
          )
        }
        res <- ElevatorControlSystem(initialState).either
        _ = assert(res.left.value.continue(asString) === "wrongElevatorNumber: 0 should be 1..16")
      } yield ()
    }
    "pickUp by idle elevator" in {
      for {
        id <- rnd[Int]
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(id -> ElevatorState(id, 0, Up, Set.empty)),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(2, Up)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.nonEmpty)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators(id).currFloor === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators(id).currFloor === 2)
          assert(s.pickUps.isEmpty)
        }
      } yield ()
    }
  }
}
