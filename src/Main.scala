import java.awt.image.BufferedImage
import java.awt.{Color, Image}

import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}
import java.awt.FlowLayout

import scala.language.reflectiveCalls

object Main extends App {

  class RgbBitmap(val dim: (Int, Int)) {
    def width = dim._1
    def height = dim._2

    private val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    def apply(x: Int, y: Int) = new Color(image.getRGB(x, y))

    def update(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB)

    def fill(c: Color) = {
      val g = image.getGraphics
      g.setColor(c)
      g.fillRect(0, 0, width, height)
    }

    def getImage(): Image = image;
  }

  case class FloatColor(r : Float, g : Float, b : Float) {
    def +(otherColor: FloatColor) = FloatColor(r + otherColor.r, g + otherColor.g, b + otherColor.b)
    def * (multiplier : Any) = multiplier match {
      case multiplier: FloatColor => FloatColor(r * multiplier.r, g * multiplier.g, b * multiplier.b)
      case multiplier: Float => FloatColor(r * multiplier, g * multiplier, b * multiplier)
    }
  }

  case class Scene(solids : List[SolidObject], lights : List[Light]) {
    def cameraPosition = Vector3D(0, 0, 0)
  }

  def clamp[T : Numeric](value: T, min: T, max: T) : T = {
    if (Numeric[T].gt(value, max))
      max
    else if (Numeric[T].lt(value, min))
      min
    else
      value
  }

  def floatColorToColor(floatColor: FloatColor) = new Color(
    clamp((floatColor.r * 255).toInt, 0, 255),
    clamp((floatColor.g * 255).toInt, 0, 255),
    clamp((floatColor.b * 255).toInt, 0, 255),
  )

  case class Vector2D(x : Float, y : Float){
    def +(otherVector: Vector2D) = Vector2D(x + otherVector.x, y + otherVector.y)
    def -(otherVector: Vector2D) = Vector2D(x - otherVector.x, y - otherVector.y)
  }

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

  case class Material(diffuse: FloatColor, specularIntensity: Float = 20.0f, mirror: Float = 0.25f);

  case class Ray(origin : Vector3D, direction : Vector3D, length : Float)

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
      val closestObject = getClosestIntersection(position, direction)
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

  def getReflectedRayDirection(originalDirection: Vector3D, normal: Vector3D): Vector3D =
    originalDirection - (normal * 2.0f * originalDirection.dot(normal))

  def generateScreenUVs(width: Int, height: Int, fov: Float): Iterable[((Int, Int), Vector3D)] = {
    val screenDistance = 1.0f / math.tan(math.toRadians(fov) / 2.0f).floatValue
    val topScreenHeight = height.toFloat / width.toFloat

    for (x <- 0 until width; y <- 0 until height) yield {
      val u = (x / (width / 2.0f)) - 1.0f
      val v = topScreenHeight * (1.0f - (y / (height / 2.0f)))
      ((x, y), Vector3D(u, v, screenDistance))
    }
  }

  def getClosestIntersection(origin : Vector3D, direction : Vector3D): Option[(SolidObject, Vector3D, Vector3D)] = {
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

  def getPixelColor(scene: Scene, origin: Vector3D): Option[FloatColor] = {
    val direction = (origin - CAMERA_POSITION).normalize
    getRayColor(scene, origin, direction, 4)
  }

  private def getRayColor(scene: Scene, origin: Vector3D, direction: Vector3D, maxDepth : Int): Option[FloatColor] = maxDepth match {
    case 0 => Some(FloatColor(0.0f, 0.0f, 0.0f))
    case _ => {
      val closestObject = getClosestIntersection(origin, direction)
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
        var reflectionColor = getRayColor(scene, intersectionPoint, reflectionRayDirection, maxDepth - 1)

        if (!reflectionColor.isDefined){
          reflectionColor = Some(FloatColor(0, 0, 0))
        }

        val rayColor = (solid.material.diffuse * lighting * (1.0f - solid.material.mirror)) + (reflectionColor.get * solid.material.mirror)

        Some(rayColor)

      }
      else {
        None
      }
    }
  }

  val FOV = 90.0f
  val WIDTH = 1280
  val HEIGHT = 720
  val CAMERA_POSITION = Vector3D(0, 0, 0)

  val rgbBitmap = new RgbBitmap(WIDTH, HEIGHT)
  rgbBitmap.fill(new Color(10, 10, 10))

  val scene = Scene(
    solids = List(
      Floor(-2.0f, Material(FloatColor(0.4f, 0.3f, 0.25f), 0.0f, 0.0f)),
      Sphere(Vector3D(-0.35f, 0.20f, 4.0f), 1.0f, Material(FloatColor(1.0f, 0.0f, 0.0f), mirror = 0.4f)),
      Sphere(Vector3D(-0.35f, 0.20f, -8.0f), 1.0f, Material(FloatColor(0.0f, 1.0f, 0.0f))),
      Sphere(Vector3D(3.5f, 1.5f, 5.5f), 1.0f, Material(FloatColor(0.0f, 1.0f, 0.0f))),
      Sphere(Vector3D(-4.5f, -1.0f, 8.0f), 1.0f, Material(FloatColor(1.0f, 0.1f, 1.0f))),
      Sphere(Vector3D(1.0f, 2.0f, 9.0f), 1.0f, Material(FloatColor(0.5f, 0.1f, 1.0f))),
      Sphere(Vector3D(2.0f, -1.0f, 7.0f), 1.0f, Material(FloatColor(0.0f, 1.0f, 1.0f))),
      Sphere(Vector3D(-5.0f, 2.0f, 12.0f), 1.0f, Material(FloatColor(1.0f, 1.0f, 1.0f))),
      Sphere(Vector3D(5.0f, 1.0f, 12.0f), 1.0f, Material(FloatColor(1.0f, 0.65f, 0.0f))),
    ),
    lights = List(
      AmbientLight(FloatColor(0.1f, 0.1f, 0.1f)),
      PointLight(Vector3D(-3.5f, 2.5f, 2.0f), FloatColor(20.0f, 20.0f, 20.0f)),
    )
  )

  def time[R](dryrun: Int, runs: Int, block: => R): R = {
    for(_ <- 0 until dryrun) {
      block
    }
    val t0 = System.nanoTime()
    for(_ <- 1 until runs) {
      block
    }
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    val diff_ns = (t1 - t0) / runs
    val diff_ms = diff_ns / 1000000.0
    println("Elapsed time: " + diff_ms + " ms")
    result
  }

  time(0, 1, {
    generateScreenUVs(WIDTH, HEIGHT, FOV)
      .map {
        case (screenCoordinates: (Int, Int), uv: Vector3D) =>
          (screenCoordinates, getPixelColor(scene, uv))
      }
      .foreach {
        case (screenCoordinates: (Int, Int), color: Some[FloatColor]) =>
          rgbBitmap.update(screenCoordinates._1, screenCoordinates._2, floatColorToColor(color.value))
        case (_: (Int, Int), None) => ()
      }
  })

  val frame = new JFrame
  frame.getContentPane.setLayout(new FlowLayout)
  frame.getContentPane.add(new JLabel(new ImageIcon(rgbBitmap.getImage())))
  frame.pack()
  frame.setVisible(true)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

}
