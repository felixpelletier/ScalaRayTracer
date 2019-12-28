package felixpelletier.scalaraytracer

case class Material(diffuse: FloatColor, specularIntensity: Float = 20.0f, mirror: Float = 0.25f);

trait SolidObject {
  def IntersectRay(origin: Vector3D, direction: Vector3D): Option[(Vector3D, Vector3D)]
  def material : Material
}

case class Floor(height : Float, material : Material) extends SolidObject {
  def IntersectRay(origin: Vector3D, direction: Vector3D): Option[(Vector3D, Vector3D)] = {
    val normal = Vector3D(0.0f, 1.0f, 0.0f)
    val planeDistance = (Vector3D(0.0f, height, 0.0f) - origin).dot(normal) / direction.dot(normal)
    if (planeDistance >= 0.0f){
      Some((origin + direction * planeDistance, normal))
    }
    else{
      None
    }
  }
}

case class Sphere(position : Vector3D, radius : Float, material : Material) extends SolidObject {
  def IntersectRay(origin: Vector3D, direction: Vector3D): Option[(Vector3D, Vector3D)] = {
    val squaredHalfIntersectionLength = math.pow(direction.dot(origin - position), 2.0) - ((origin - position).length2() - (radius*radius))
    if (squaredHalfIntersectionLength < 0.0f) return None

    val halfIntersectionLength = math.sqrt(squaredHalfIntersectionLength).toFloat
    val centerIntersectionDistance = 0.0f - direction.dot(origin - position)
    val closestIntersectionDistance = centerIntersectionDistance - halfIntersectionLength

    if (closestIntersectionDistance < 0.0f) return None

    val intersectionPoint = origin + direction * closestIntersectionDistance.toFloat
    val normal = (intersectionPoint - position).normalize

    Some(intersectionPoint, normal)
  }
}
