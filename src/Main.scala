import java.awt.image.BufferedImage
import java.awt.{Color, Image}

import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}
import java.awt.FlowLayout

import Main.getClosestIntersection

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
    def scale(scalar : Float) = FloatColor(r * scalar, g * scalar, b * scalar)
  }

  case class Scene(solids : List[SolidObject], lights : List[Light]) {
    def cameraPosition : Vector3D = Vector3D(0, 0, 0)
  }

  def constraint(value: Int, min: Int, max: Int) : Int = {
    if (value > max)
      max
    else if (value < min)
      min
    else
      value
  }

  def floatColorToColor(floatColor: FloatColor) = new Color(
    constraint((floatColor.r * 255).toInt, 0, 255),
    constraint((floatColor.g * 255).toInt, 0, 255),
    constraint((floatColor.b * 255).toInt, 0, 255),
  )

  case class Vector2D(x : Float, y : Float){
    def +(otherVector: Vector2D) : Vector2D = Vector2D(x + otherVector.x, y + otherVector.y)
    def -(otherVector: Vector2D) : Vector2D = Vector2D(x - otherVector.x, y - otherVector.y)
  }

  case class Vector3D(x : Float, y : Float, z : Float) {

    def +(otherVector: Vector3D) = Vector3D(x + otherVector.x, y + otherVector.y, z + otherVector.z)

    def -(otherVector: Vector3D) = Vector3D(x - otherVector.x, y - otherVector.y, z - otherVector.z)

    def *(scalar: Float) = Vector3D(scalar * x, scalar * y, scalar * z)

    def length2() : Float = {
      x*x + y*y + z*z
    }

    def length() : Float = {
      math.sqrt(length2()).toFloat
    }

    def dot(otherVector : Vector3D) : Float = {
      x * otherVector.x + y * otherVector.y + z * otherVector.z
    }

    def normalize : Vector3D = {
      val l = length()
      Vector3D(x / l, y / l, z/l)
    }

  }

  case class Ray(origin : Vector3D, direction : Vector3D, length : Float)

  trait SolidObject {
    def IntersectRay(origin: Vector3D, direction: Vector3D): Option[Vector3D]
    def color : FloatColor
  }

  case class Floor(height : Float, color : FloatColor) extends SolidObject {
    def IntersectRay(origin: Vector3D, direction: Vector3D): Option[Vector3D] = {
      val normal = Vector3D(0.0f, 1.0f, 0.0f)
      val planeDistance = (Vector3D(0.0f, height, 0.0f) - origin).dot(normal) / direction.dot(normal)
      if (planeDistance >= 0.0f){
        Some(origin + direction * planeDistance)
      }
      else{
        None
      }
    }
  }

  case class Sphere(position : Vector3D, radius : Float, color : FloatColor) extends SolidObject {

    def IntersectRay(origin: Vector3D, direction: Vector3D): Option[Vector3D] = {
      val squaredHalfIntersectionLength = math.pow(direction.dot(origin - position), 2.0) - ((origin - position).length2() - (radius*radius))
      if (squaredHalfIntersectionLength < 0.0f) return None

      val halfIntersectionLength = math.sqrt(squaredHalfIntersectionLength)
      val centerIntersectionDistance = 0 - direction.dot(origin - position)
      val closestIntersectionDistance = centerIntersectionDistance - halfIntersectionLength

      Some(origin + direction * closestIntersectionDistance.toFloat)
    }
  }

  trait Light {
    def CalculateLighting(scene : Scene, point : Vector3D) : FloatColor
  }

  case class AmbientLight(color : FloatColor) extends Light {
    def CalculateLighting(scene : Scene, point : Vector3D) = color
  }

  case class PointLight(position : Vector3D, color : FloatColor) extends Light {
    def CalculateLighting(scene : Scene, point : Vector3D): FloatColor = {

      val direction = (point - position).normalize
      val closestObject = getClosestIntersection(position, direction)
      if (closestObject.isDefined) {
        if ((point - closestObject.get._2).length() < 0.001F) {
          val distance_squared = (position - point).length2()
          val lighting = 1.0f / distance_squared
          color * lighting
        }
        else {
          FloatColor(0.0f, 0.0f, 0.0f)
        }
      }
      else {
        FloatColor(0.0f, 0.0f, 0.0f)
      }
    }
  }

  def generateScreenUVs(width: Int, height: Int, fov: Float): Iterable[((Int, Int), Vector3D)] = {
    val screenDistance = math.tan(FOV / 2).floatValue
    val topScreenHeight = HEIGHT.toFloat / WIDTH.toFloat

    for (x <- 0 until width; y <- 0 until height) yield {
      val u = (x / (width / 2.0f)) - 1.0f
      val v = topScreenHeight * (((height/2.0f) - y) / (height / 2.0f)) // TODO: Simplify this
      ((x, y), Vector3D(u, v, screenDistance))
    }
  }

  def getClosestIntersection(origin : Vector3D, direction : Vector3D): Option[(SolidObject, Vector3D)] ={
    scene.solids.map{case (obj : SolidObject) => (obj, obj.IntersectRay(origin, direction)) }
      .filter(_._2.isDefined)
      .map {
        case (solid: SolidObject, intersection : Some[Vector3D]) => (solid, intersection.get)
      }
      .minByOption {
        case (_: SolidObject, intersection : Vector3D) => {
          (intersection - origin).length2()
        }
      }
  }

  def getPixelColor(scene: Scene, origin: Vector3D): Option[FloatColor] = {
    val direction = (origin - CAMERA_POSITION).normalize
    val closestObject = getClosestIntersection(origin, direction)
    if (closestObject.isDefined) {
      val intersectionPoint = closestObject.get._2
      val lighting = scene.lights.map {_.CalculateLighting(scene, intersectionPoint)}
        .fold(FloatColor(0,0,0)){_ + _}

      val objectColor = closestObject.get._1.color
      Some(objectColor * lighting)
    }
    else {
      None
    }
  }

  val FOV = 90.0f
  val WIDTH = 1280
  val HEIGHT = 720
  val CAMERA_POSITION = Vector3D(0, 0, 0)

  val rgbBitmap = new RgbBitmap(WIDTH, HEIGHT)
  rgbBitmap.fill(new Color(25, 25, 25))

  val scene = Scene(
    solids = List(
      Floor(-2.0f, FloatColor(0.25f, 0.25f, 0.25f)),
      Sphere(Vector3D(0, 0, 5.0f), 1.0f, FloatColor(1.0f, 0.0f, 0.0f)),
      Sphere(Vector3D(2.5f, 0.0f, 7.5f), 1.0f, FloatColor(0.0f, 1.0f, 0.0f)),
      Sphere(Vector3D(-2.5f, -1.0f, 10.0f), 1.0f, FloatColor(1.0f, 0.1f, 1.0f)),
      Sphere(Vector3D(0.1f, 1.0f, 7.5f), 1.0f, FloatColor(0.5f, 0.1f, 1.0f)),
      Sphere(Vector3D(1.0f, -1.0f, 9.0f), 1.0f, FloatColor(0.0f, 1.0f, 1.0f)),
      Sphere(Vector3D(-5.0f, 1.0f, 15.0f), 1.0f, FloatColor(1.0f, 1.0f, 1.0f)),
    ),
    lights = List(
      AmbientLight(FloatColor(0.25f, 0.25f, 0.25f)),
      PointLight(Vector3D(-2.5f, 1.5f, 4.0f), FloatColor(10.0f, 10.0f, 10.0f))
    )
  )

  generateScreenUVs(WIDTH, HEIGHT, FOV)
    .map {
      case (screenCoordinates: (Int, Int), uv: Vector3D) =>
        (screenCoordinates, getPixelColor(scene, uv))
    }
    .map {
      case (screenCoordinates: (Int, Int), color: Some[FloatColor]) =>
      rgbBitmap.update(screenCoordinates._1, screenCoordinates._2, floatColorToColor(color.value))
      case (_: (Int, Int), None) => ()
    }

  val frame = new JFrame
  frame.getContentPane.setLayout(new FlowLayout)
  frame.getContentPane.add(new JLabel(new ImageIcon(rgbBitmap.getImage())))
  frame.pack()
  frame.setVisible(true)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

}
