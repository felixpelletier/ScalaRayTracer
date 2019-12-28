import Utils.clamp
import Rays.{getClosestIntersection, getReflectedRayDirection}

trait Light {
  def CalculateLighting(scene : Scene, solid: SolidObject, point : Vector3D, normal : Vector3D) : FloatColor
}

case class AmbientLight(color : FloatColor) extends Light {
  def CalculateLighting(scene : Scene, solid : SolidObject, point : Vector3D, normal : Vector3D): FloatColor =
    color * solid.material.diffuse
}

case class PointLight(position : Vector3D, lightColor : FloatColor) extends Light {
  def CalculateLighting(scene : Scene, solid : SolidObject, intersection : Vector3D, normal : Vector3D): FloatColor = {

    val direction = (intersection - position).normalize
    val closestObject = getClosestIntersection(scene, position, direction)
    if (closestObject.isDefined) {
      if ((intersection - closestObject.get._2).length2() < 0.000001F) {
        val distance_squared = (position - intersection).length2()
        val lighting = 1.0f / distance_squared
        val angleAttenuation = -(direction.dot(normal))

        val reflectedRayDirection = getReflectedRayDirection(direction, normal)
        val intersectionToCamera = (scene.cameraPosition - intersection).normalize
        val specularValue = reflectedRayDirection.dot(intersectionToCamera)

        val specularIntensity = if (specularValue > 0.0f)
          clamp(math.pow(specularValue, 5.0f).floatValue * lighting * solid.material.specularIntensity, 0.0f, 1.0f)
        else 0.0f

        val diffuse = lightColor * lighting * angleAttenuation;
        val specular = FloatColor(specularIntensity, specularIntensity, specularIntensity)

        return (diffuse + specular)
      }
    }

    FloatColor(0.0f, 0.0f, 0.0f)
  }
}
