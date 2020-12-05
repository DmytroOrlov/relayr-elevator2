package elevator

import capture.Capture
import zio._

trait ElevatorControlSystem

object ElevatorControlSystem {

  sealed trait State

  case object Idle extends State

  sealed trait Direction extends State

  case object Up extends Direction

  case object Down extends Direction

  type ElevatorId = Int
  type Start = Int
  type Target = Int

  def apply(elevators: Seq[(ElevatorId, Start, Target)]): IO[Capture[ElevatorErr], ElevatorControlSystem] =
    for {
      _ <- IO.fail(ElevatorErr.maxElevatorExceeded(elevators.length))
        .when(elevators.length > 16)
    } yield ???
}

case class AppCfg(elevatorMax: Int)
