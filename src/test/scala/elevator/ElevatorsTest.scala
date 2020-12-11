package elevator

import elevator.ElevatorErr._
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
    "pickUp(Up) by idle elevator" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 0, Up, Set.empty),
              22 -> ElevatorState(22, 0, Up, Set.empty),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(2, Up)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.nonEmpty)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.currFloor == 2) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 0)
          assert(s.pickUps.isEmpty)
        }
      } yield ()
    }
    "pickUp(Down) by idle elevator" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 0, Up, Set.empty),
              22 -> ElevatorState(22, 0, Up, Set.empty),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(2, Down)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.nonEmpty)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.currFloor == 2) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 0)
          assert(s.pickUps.isEmpty)
        }
      } yield ()
    }
    "pickUp by already moving AND passing by AND moving same direction elevator" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 0, Up, Set.empty),
              22 -> ElevatorState(22, 0, Up, Set.empty),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(3, Down)
        _ <- ecs.step()
        _ <- ecs.pickUp(2, Up)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.size === 2)
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.size === 1)
          assert(s.elevators.values.count(_.currFloor == 2) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
        }
      } yield ()
    }
    "not pickUp by already moving AND passing by AND moving different direction elevator" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 0, Up, Set.empty),
              22 -> ElevatorState(22, 0, Up, Set.empty),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(3, Down)
        _ <- ecs.step()
        _ <- ecs.pickUp(2, Down)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.size === 2)
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
          assert(s.elevators.values.count(_.dropOffs.nonEmpty) === 2)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.size === 2)
          assert(s.elevators.values.count(_.currFloor == 2) === 1)
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
        }
      } yield ()
    }
    "pickUp(Up) with idle same floor" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 1, Up, Set.empty),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(1, Up)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.isEmpty)
          assert(s.elevators.values.count(_.dropOffs.isEmpty) === 1)
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
        }
      } yield ()
    }
    "pickUp(Down) with idle same floor" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 1, Up, Set.empty),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.pickUp(1, Down)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.isEmpty)
          assert(s.elevators.values.count(_.dropOffs.isEmpty) === 1)
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
        }
      } yield ()
    }
    "continue moving same direction and return" in {
      for {
        ecsState <- Ref.make {
          EcsState(
            pickUps = Set.empty,
            elevators = Map(
              11 -> ElevatorState(11, 1, Up, Set(3)),
            ),
          )
        }
        ecs <- ElevatorControlSystem(ecsState)
        _ <- ecs.dropOff(11, 0)
        s <- ecs.status
        _ <- IO {
          assert(s.pickUps.isEmpty)
          assert(s.elevators.values.count(_.dropOffs.size == 2) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.dropOffs.size == 2) === 1)
          assert(s.elevators.values.count(_.currFloor == 2) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.dropOffs.size == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 3) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.dropOffs.size == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 2) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.dropOffs.size == 1) === 1)
          assert(s.elevators.values.count(_.currFloor == 1) === 1)
        }
        _ <- ecs.step()
        s <- ecs.status
        _ <- IO {
          assert(s.elevators.values.count(_.dropOffs.isEmpty) === 1)
          assert(s.elevators.values.count(_.currFloor == 0) === 1)
        }
      } yield ()
    }
  }
}
