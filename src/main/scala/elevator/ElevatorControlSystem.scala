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

case class ElevatorState(id: ElevatorId, currFloor: Floor, direction: Direction, dropOffs: Set[Floor])

case class EcsState(pickUps: Set[(Floor, Direction)], elevators: Map[ElevatorId, ElevatorState])

object ElevatorControlSystem {

  sealed trait Elevator

  case object Idle extends Elevator

  sealed trait Direction extends Elevator

  case object Up extends Direction

  case object Down extends Direction

  type Floor = Int
  type ElevatorId = Int

  def apply(ecsState: Ref[EcsState]): IO[Capture[ElevatorErr], ElevatorControlSystem] =
    for {
      initial <- ecsState.get
      _ <- IO.fail(ElevatorErr.maxElevatorExceeded(initial.elevators.size))
        .when(initial.elevators.size > 16)
      res <- IO.succeed {
        new ElevatorControlSystem {
          def step() = for {
            _ <- zio.IO.unit
          } yield ???

          def dropOff(id: ElevatorId, floor: Floor) =
            ecsState.updateSome {
              case s if s.elevators.contains(id) =>
                val e = s.elevators(id)
                s.copy(elevators = s.elevators + (e.id -> e.copy(dropOffs = e.dropOffs + floor)))
            }

          def pickUp(floor: Floor, direction: Direction) =
            ecsState.update { s =>
              val withPickUps = s.copy(pickUps = s.pickUps + (floor -> direction))

              if (!shouldStop(s, floor, direction)) {
                existsIdle(s).map { e =>
                  e.copy(dropOffs = e.dropOffs + floor, direction = if (e.currFloor < floor) Up else Down)
                }.fold(withPickUps) { e =>
                  withPickUps.copy(elevators = withPickUps.elevators + (e.id -> e))
                }
              } else withPickUps
            }

          def status = ecsState.get

          def shouldStop(state: EcsState, floor: Floor, direction: Direction) =
            state.elevators.values.exists { e =>
              direction == e.direction &&
                (direction == Up && e.dropOffs.max >= floor || direction == Down && e.dropOffs.min <= floor)
            }

          def existsIdle(state: EcsState) =
            state.elevators.values.collectFirst { case e if e.dropOffs.isEmpty => e }
        }
      }
    }
      yield res
}

case class AppCfg(elevatorMax: Int)
