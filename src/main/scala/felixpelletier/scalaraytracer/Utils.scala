package felixpelletier.scalaraytracer

object Utils {

  def clamp[T : Numeric](value: T, min: T, max: T) : T = {
      if (Numeric[T].gt(value, max))
          max
      else if (Numeric[T].lt(value, min))
          min
      else
          value
  }

}
