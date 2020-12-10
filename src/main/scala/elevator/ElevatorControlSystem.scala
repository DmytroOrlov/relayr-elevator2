package elevator

import capture.Capture
import elevator.ElevatorControlSystem._
import zio._

trait ElevatorControlSystem

case class ElevatorState(id: ElevatorId, currFloor: Floor, CurrDirection: Direction, dropOffs: Set[Floor])

case class EcsState(pickUps: Set[(Floor, Direction)], elevators: Seq[ElevatorState])

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

  def apply(initialState: EcsState): IO[Capture[ElevatorErr], ElevatorControlSystem] =
    for {
      _ <- IO.fail(ElevatorErr.maxElevatorExceeded(initialState.elevators.length))
        .when(initialState.elevators.length > 16)
    } yield ???
}

case class AppCfg(elevatorMax: Int)
