
case object Rays {

  def getReflectedRayDirection(originalDirection: Vector3D, normal: Vector3D): Vector3D =
    originalDirection - (normal * 2.0f * originalDirection.dot(normal))

  def getClosestIntersection(scene: Scene, origin : Vector3D, direction : Vector3D): Option[(SolidObject, Vector3D, Vector3D)] = {
    scene.solids.map{case (obj : SolidObject) => (obj, obj.IntersectRay(origin, direction)) }
      .filter(_._2.isDefined)
      .map {
        case (solid: SolidObject, intersection : Some[(Vector3D, Vector3D)]) => (solid, intersection.get._1, intersection.get._2)
      }
      .minByOption {
        case (_: SolidObject, intersection : Vector3D, _ : Vector3D) => {
          (intersection - origin).length2()
        }
      }
  }

  def getRayColor(scene: Scene, origin: Vector3D, direction: Vector3D, maxDepth : Int): FloatColor = maxDepth match {
    case 0 => FloatColor(0.0f, 0.0f, 0.0f)
    case _ => {
      val closestObject = getClosestIntersection(scene, origin, direction)
      if (closestObject.isDefined) {
        val intersectionPoint = closestObject.get._2
        val normal = closestObject.get._3
        val solid = closestObject.get._1
        val lighting = scene.lights.map {
          _.CalculateLighting(scene, solid, intersectionPoint, normal)
        }
          .fold(FloatColor(0, 0, 0)) {
            _ + _
          }

        val reflectionRayDirection = getReflectedRayDirection(direction, normal)
        val reflectionColor = getRayColor(scene, intersectionPoint, reflectionRayDirection, maxDepth - 1)

        (solid.material.diffuse * lighting * (1.0f - solid.material.mirror)) + (reflectionColor * solid.material.mirror)
      }
      else {
        Constants.WORLD_COLOR
      }
    }
  }

}
