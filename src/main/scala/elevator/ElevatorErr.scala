package elevator

import capture.Capture
import capture.Capture.Constructors

trait ElevatorErr[+A] {
  def wrongElevatorNumber(number: Int): A

  def throwable(message: String)(e: Throwable): A

  def message(message: String): A
}

object ElevatorErr extends Constructors[ElevatorErr] {
  def wrongElevatorNumber(number: Int) =
    Capture[ElevatorErr](_.wrongElevatorNumber(number))

  def throwable(message: String)(e: Throwable) =
    Capture[ElevatorErr](_.throwable(message)(e))

  def message(message: String) =
    Capture[ElevatorErr](_.message(message))

  trait AsString extends ElevatorErr[String] {
    def wrongElevatorNumber(number: Int) =
      s"wrongElevatorNumber: $number should be 1..16"

    def throwable(message: String)(e: Throwable) =
      s"$message: ${e.getMessage}"

    def message(message: String) =
      message
  }

  val asString = new AsString {}
}
