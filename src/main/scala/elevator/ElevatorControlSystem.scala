package elevator

import capture.Capture
import zio._

trait ElevatorControlSystem {
  def status: UIO[EcsState]

  def pickUp(floor: Floor, direction: Direction): UIO[Unit]

  def dropOff(id: ElevatorId, floor: Floor): UIO[Unit]

  def step(): UIO[Unit]
}

case class ElevatorState(id: ElevatorId, currFloor: Floor, direction: Direction, dropOffs: Set[Floor])

case class EcsState(pickUps: Set[(Floor, Direction)], elevators: Map[ElevatorId, ElevatorState])

sealed trait Direction

case object Up extends Direction

case object Down extends Direction

object ElevatorControlSystem {
  def apply(ecsState: Ref[EcsState]): IO[Capture[ElevatorErr], ElevatorControlSystem] =
    for {
      initial <- ecsState.get
      _ <- IO.fail(ElevatorErr.wrongElevatorNumber(initial.elevators.size))
        .when(initial.elevators.size > 16 || initial.elevators.size < 1)
      res = new ElevatorControlSystem {
        private def sameDir(e: ElevatorState) =
          e.direction == Up && e.dropOffs.exists(_ > e.currFloor) ||
            e.direction == Down && e.dropOffs.exists(_ < e.currFloor)

        private def nextFloor(e: ElevatorState) =
          if (e.direction == Up) e.currFloor + 1 else e.currFloor - 1

        private def reverseFloor(e: ElevatorState) =
          if (e.direction == Up) e.currFloor - 1 else e.currFloor + 1

        def step() = ecsState.update { s =>
          var doorsOpenAt = Set.empty[Int]
          var elevatorsStep = s.elevators.map {
            case (id, e@ElevatorState(_, _, dir, dropOffs)) if dropOffs.nonEmpty =>
              val newFloor = if (sameDir(e)) nextFloor(e) else reverseFloor(e)
              val newDropOffs = dropOffs - newFloor
              if (newDropOffs != dropOffs) doorsOpenAt += newFloor
              id -> ElevatorState(id, newFloor, dir, newDropOffs)
            case e => e
          }

          val newPickUps =
            s.pickUps
              .removedAll(elevatorsStep.values.map(e => e.currFloor -> e.direction))
              .filterNot { case (fl, _) => doorsOpenAt.contains(fl) }

          val delayedPickUps = newPickUps.filter { case (floor, direction) => !someoneShouldStop(elevatorsStep, floor, direction) }
          val idleElevators = elevatorsStep.values.filter(_.dropOffs.isEmpty)
          if (idleElevators.nonEmpty && delayedPickUps.nonEmpty) {
            delayedPickUps.foldLeft(idleElevators.toSet) {
              case (idleElevators, (fl, dir)) if idleElevators.nonEmpty =>
                val closestElevator = idleElevators.minBy(e => (e.currFloor - fl).abs)
                elevatorsStep = elevatorsStep + (closestElevator.id -> idleStartsMoving(closestElevator, fl, dir))
                idleElevators - closestElevator
              case (ps, _) => ps
            }
          }
          EcsState(
            pickUps = newPickUps,
            elevators = elevatorsStep,
          )
        }

        def dropOff(id: ElevatorId, floor: Floor) =
          ecsState.updateSome {
            case s if s.elevators.contains(id) =>
              val e = s.elevators(id)
              s.copy(elevators = s.elevators + (e.id -> e.copy(dropOffs = e.dropOffs + floor)))
          }

        def pickUp(floor: Floor, direction: Direction) =
          ecsState.update { s =>
            val updatedPickUpsState = s.copy(pickUps = s.pickUps + (floor -> direction))

            if (!someoneShouldStop(s.elevators, floor, direction)) {
              collectClosestIdleElevator(s, floor).map { e =>
                idleStartsMoving(e, floor, direction)
              }.fold(updatedPickUpsState) { idleElevator =>
                if (idleElevator.dropOffs.nonEmpty) updatedPickUpsState.copy(elevators = updatedPickUpsState.elevators + (idleElevator.id -> idleElevator))
                else s
              }
            } else updatedPickUpsState
          }

        def status = ecsState.get

        private def someoneShouldStop(elevators: Map[ElevatorId, ElevatorState], floor: Floor, direction: Direction) =
          elevators.values.exists { e =>
            e.dropOffs.contains(floor) || e.dropOffs.nonEmpty && direction == e.direction &&
              (direction == Up && e.dropOffs.max >= floor || direction == Down && e.dropOffs.min <= floor)
          }

        private def collectClosestIdleElevator(state: EcsState, floor: Floor) =
          state.elevators.values.filter(_.dropOffs.isEmpty).minByOption(_.currFloor - floor)

        private def idleStartsMoving(e: ElevatorState, floor: Floor, direction: Direction) =
          if (floor == e.currFloor) e
          else e.copy(dropOffs = e.dropOffs + floor, direction = if (e.currFloor < floor) Up else Down)
      }
    } yield res
}
