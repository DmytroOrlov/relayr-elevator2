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
        private def sameDir(e: ElevatorState) =
          e.direction == Up && e.dropOffs.exists(_ > e.currFloor) ||
            e.direction == Down && e.dropOffs.exists(_ < e.currFloor)


        def step() = ecsState.update { s =>
          val es = s.elevators.map {
            case (id, e@ElevatorState(_, curr, dir, dropOffs)) if dropOffs.nonEmpty && sameDir(e) =>

              ???
            case e => e
          }

          EcsState(s.pickUps.removedAll(es.values.map(e => e.currFloor -> e.direction)), es)
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
              collectClosestIdleElevator(s, floor).map { e =>
                idleStartsMoving(e, floor, direction)
              }.fold(updatedPickUps) { idleElevator =>
                updatedPickUps.copy(elevators = updatedPickUps.elevators + (idleElevator.id -> idleElevator))
              }
            } else updatedPickUps
          }

        def status = ecsState.get

        private def someoneShouldStop(state: EcsState, floor: Floor, direction: Direction) =
          state.elevators.values.exists { e =>
            direction == e.direction &&
              (direction == Up && e.dropOffs.max >= floor || direction == Down && e.dropOffs.min <= floor)
          }

        private def collectClosestIdleElevator(state: EcsState, floor: Floor) =
          state.elevators.values.filter(_.dropOffs.isEmpty).minByOption(_.currFloor - floor)

        private def idleStartsMoving(e: ElevatorState, floor: Floor, direction: Direction) =
          e.copy(dropOffs = e.dropOffs + floor, direction = if (e.currFloor < floor) Up else Down)
      }
    } yield res
}

case class AppCfg(elevatorMax: Int)
