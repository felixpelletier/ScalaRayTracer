case class Vector3D(x : Float, y : Float, z : Float) {

  def +(otherVector: Vector3D) = Vector3D(x + otherVector.x, y + otherVector.y, z + otherVector.z)
  def -(otherVector: Vector3D) = Vector3D(x - otherVector.x, y - otherVector.y, z - otherVector.z)
  def *(scalar: Float) = Vector3D(scalar * x, scalar * y, scalar * z)

  def length2(): Float = x*x + y*y + z*z
  def length(): Float = math.sqrt(length2()).toFloat
  def dot(otherVector : Vector3D): Float = x * otherVector.x + y * otherVector.y + z * otherVector.z
  def inverse(): Vector3D = Vector3D(-x, -y, -z)

  def normalize : Vector3D = {
    val l = length()
    if (l > 0.0f) Vector3D(x / l, y / l, z / l) else Vector3D(0.0f, 0.0f, 0.0f)
  }

}
