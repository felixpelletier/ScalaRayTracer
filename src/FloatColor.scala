case class FloatColor(r : Float, g : Float, b : Float) {
  def +(otherColor: FloatColor) = FloatColor(r + otherColor.r, g + otherColor.g, b + otherColor.b)
  def * (multiplier : Any) = multiplier match {
    case multiplier: FloatColor => FloatColor(r * multiplier.r, g * multiplier.g, b * multiplier.b)
    case multiplier: Float => FloatColor(r * multiplier, g * multiplier, b * multiplier)
  }
}
