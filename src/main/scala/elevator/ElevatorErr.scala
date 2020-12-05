package elevator

import capture.Capture
import capture.Capture.Constructors

trait ElevatorErr[+A] {
  def maxElevatorExceeded(number: Int): A

  def throwable(message: String)(e: Throwable): A

  def message(message: String): A
}

object ElevatorErr extends Constructors[ElevatorErr] {
  def maxElevatorExceeded(number: Int) =
    Capture[ElevatorErr](_.maxElevatorExceeded(number))

  def throwable(message: String)(e: Throwable) =
    Capture[ElevatorErr](_.throwable(message)(e))

  def message(message: String) =
    Capture[ElevatorErr](_.message(message))

  trait AsString extends ElevatorErr[String] {
    def maxElevatorExceeded(number: Int) =
      s"maxElevatorExceeded $number"

    def throwable(message: String)(e: Throwable) =
      s"$message: ${e.getMessage}"

    def message(message: String) =
      message
  }

  val asString = new AsString {}
}
