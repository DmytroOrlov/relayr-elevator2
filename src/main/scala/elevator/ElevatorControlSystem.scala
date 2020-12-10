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
      res = new ElevatorControlSystem {
        def step() = ecsState.update { s =>


          s
        }

        def dropOff(id: ElevatorId, floor: Floor) =
          ecsState.updateSome {
            case s if s.elevators.contains(id) =>
              val e = s.elevators(id)
              s.copy(elevators = s.elevators + (e.id -> e.copy(dropOffs = e.dropOffs + floor)))
          }

        def pickUp(floor: Floor, direction: Direction) =
          ecsState.update { s =>
            val updatedPickUps = s.copy(pickUps = s.pickUps + (floor -> direction))

            if (!someoneShouldStop(s, floor, direction)) {
              collectIdleElevator(s, floor).map { e =>
                idleStartsMoving(e, floor, direction)
              }.fold(updatedPickUps) { idleElevator =>
                updatedPickUps.copy(elevators = updatedPickUps.elevators + (idleElevator.id -> idleElevator))
              }
            } else updatedPickUps
          }

        def status = ecsState.get

        def someoneShouldStop(state: EcsState, floor: Floor, direction: Direction) =
          state.elevators.values.exists { e =>
            direction == e.direction &&
              (direction == Up && e.dropOffs.max >= floor || direction == Down && e.dropOffs.min <= floor)
          }

        def collectIdleElevator(state: EcsState, floor: Floor) =
          state.elevators.values.filter(_.dropOffs.isEmpty).minByOption(_.currFloor - floor)

        def idleStartsMoving(e: ElevatorState, floor: Floor, direction: Direction) =
          e.copy(dropOffs = e.dropOffs + floor, direction = if (e.currFloor < floor) Up else Down)
      }
    } yield res
}

case class AppCfg(elevatorMax: Int)
