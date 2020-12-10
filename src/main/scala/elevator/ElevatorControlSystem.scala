package elevator

import capture.Capture
import elevator.ElevatorControlSystem._
import zio._

trait ElevatorControlSystem {
  def status: UIO[EcsState]

  def pickUp(floor: Floor, direction: Direction): UIO[Unit]

  def dropOff(id: ElevatorId, floor: Floor): UIO[Unit]

  def step(): UIO[Unit]
}

case class ElevatorState(id: ElevatorId, currFloor: Floor, CurrDirection: Direction, dropOffs: Set[Floor])

case class EcsState(pickUps: Set[(Floor, Direction)], elevators: Map[ElevatorId, ElevatorState])

object ElevatorControlSystem {

  sealed trait Elevator

  case object Idle extends Elevator

  sealed trait Direction extends Elevator

  case object Up extends Direction

  case object Down extends Direction

  type Floor = Int
  type ElevatorId = Int
  type Start = Int
  type Target = Int

  def apply(ecsState: Ref[EcsState]): IO[Capture[ElevatorErr], ElevatorControlSystem] =
    for {
      initial <- ecsState.get
      _ <- IO.fail(ElevatorErr.maxElevatorExceeded(initial.elevators.length))
        .when(initial.elevators.length > 16)
      res <- IO.succeed {
        new ElevatorControlSystem {
          def status = ecsState.get

          def pickUp(floor: Floor, direction: Direction) =
            ecsState.update(s => s.copy(pickUps = s.pickUps + (floor -> direction)))

          def dropOff(id: ElevatorId, floor: Floor) =
            for {
              _ <- ecsState.update { s =>
                s.elevators
              }
            } yield ()

          def step() = ???
        }
      }
    } yield res
}

case class AppCfg(elevatorMax: Int)
