package felixpelletier.scalaraytracer

import java.awt.image.RenderedImage
import java.awt.{Color, FlowLayout, Image}
import java.io.{File, IOException}

import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}

import scala.collection.parallel.CollectionsHaveToParArray

object Main extends App {

  val FOV = 90.0f
  val WIDTH = 1280
  val HEIGHT = 720
  val CAMERA_POSITION = Vector3D(0.0f, 0.0f, 0.0f)
  val SUPER_SAMPLING = 1;

  def generateScreenUVs(width: Int, height: Int, fov: Float): Iterable[((Int, Int), Vector3D)] = {
    val screenDistance = 1.0f / math.tan(math.toRadians(fov) / 2.0f).floatValue
    val topScreenHeight = height.toFloat / width.toFloat

    for (x <- 0 until width; y <- 0 until height) yield {
      val u = (x / (width / 2.0f)) - 1.0f
      val v = topScreenHeight * (1.0f - (y / (height / 2.0f)))
      ((x, y), Vector3D(u + CAMERA_POSITION.x, v + CAMERA_POSITION.y, screenDistance + CAMERA_POSITION.z))
    }
  }

  def getPixelColor(scene: Scene, origin: Vector3D): FloatColor = {
    val direction = (origin - CAMERA_POSITION).normalize
    Rays.getRayColor(scene, CAMERA_POSITION, direction, 3)
  }

  val scene = Scene(
    solids = List(
      Floor(-2.0f, Material(FloatColor(0.9f, 0.9f, 0.9f), 0.0f, mirror = 0.0f)),
      Sphere(Vector3D(-0.35f, 0.20f, 4.0f), 1.0f, Material(FloatColor(1.0f, 0.0f, 0.0f), mirror = 0.4f)),
      Sphere(Vector3D(3.5f, 1.5f, 5.5f), 1.0f, Material(FloatColor(0.0f, 1.0f, 0.0f))),
      Sphere(Vector3D(-4.5f, -1.0f, 8.0f), 1.0f, Material(FloatColor(1.0f, 0.1f, 1.0f))),
      Sphere(Vector3D(1.0f, 2.0f, 9.0f), 1.25f, Material(FloatColor(0.5f, 0.1f, 1.0f))),
      Sphere(Vector3D(2.0f, -1.0f, 7.0f), 0.75f, Material(FloatColor(0.0f, 1.0f, 1.0f))),
      Sphere(Vector3D(4.0f, -1.0f, 6.0f), 1.0f, Material(FloatColor(0.95f, 0.95f, 0.95f), mirror = 0.9f)),
      Sphere(Vector3D(-3.0f, -1.0f, 5.0f), 0.5f, Material(FloatColor(0.95f, 0.95f, 0.95f), mirror = 0.1f)),
      Sphere(Vector3D(-5.0f, 2.0f, 12.0f), 1.0f, Material(FloatColor(1.0f, 1.0f, 1.0f))),
      Sphere(Vector3D(5.0f, 1.0f, 12.0f), 1.25f, Material(FloatColor(1.0f, 0.65f, 0.0f))),

      Sphere(Vector3D(-0.35f, 0.20f, -2.0f), 1.0f, Material(FloatColor(1.0f, 1.0f, 0.0f))),
      Sphere(Vector3D(2.00f, 1.00f, -1.0f), 1.0f, Material(FloatColor(0.0f, 1.0f, 0.0f))),
      Sphere(Vector3D(-4.35f, 2.20f, -0.0f), 1.0f, Material(FloatColor(0.5f, 1.0f, 0.5f))),
    ),
    lights = List(
      AmbientLight(FloatColor(0.1f, 0.1f, 0.1f)),
      PointLight(Vector3D(-5.25f, 3.75f, 3.0f), FloatColor(30.0f, 30.0f, 30.0f)),
    )
  )

  def floatColorToColor(floatColor: FloatColor) = new Color(
    Utils.clamp((floatColor.r * 255).toInt, 0, 255),
    Utils.clamp((floatColor.g * 255).toInt, 0, 255),
    Utils.clamp((floatColor.b * 255).toInt, 0, 255),
  )

  def time[R](warmupRuns: Int, runs: Int, block: => R): R = {
    for(_ <- 0 until warmupRuns) {
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

  val rgbBitmap = new RgbBitmap(WIDTH, HEIGHT)
  val rgbFloatBitmap = Array.ofDim[FloatColor](WIDTH, HEIGHT)
  for(x <- 0 until WIDTH; y <- 0 until HEIGHT) yield {
    rgbFloatBitmap(x)(y) = FloatColor(0,0,0)
  }

  time(0, 1, {
    generateScreenUVs(WIDTH*SUPER_SAMPLING, HEIGHT*SUPER_SAMPLING, FOV)
      .toParArray
      .map {
        case (screenCoordinates: (Int, Int), uv: Vector3D) =>
          (screenCoordinates, getPixelColor(scene, uv))
      }
      .foreach {
        case (screenCoordinates: (Int, Int), color: FloatColor) =>
          val x = screenCoordinates._1 / SUPER_SAMPLING
          val y = screenCoordinates._2 / SUPER_SAMPLING
          rgbFloatBitmap(x)(y) = rgbFloatBitmap(x)(y) + (color * (1.0f/(SUPER_SAMPLING*SUPER_SAMPLING)))
      }

    for(x <- 0 until WIDTH; y <- 0 until HEIGHT) yield {
      rgbBitmap.update(x, y, floatColorToColor(rgbFloatBitmap(x)(y)) )
    }
  })

  def showImage(image: Image) {
    val frame = new JFrame
    frame.getContentPane.setLayout(new FlowLayout)
    frame.getContentPane.add(new JLabel(new ImageIcon(image)))
    frame.pack()
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  }

  def saveImage(image: RenderedImage, filename: String): Unit = {
    try {
      var actualFilename = filename.trim()
      if (actualFilename.endsWith(".jpg")){
        actualFilename = filename.substring(0, filename.lastIndexOf(".jpg"))
      }
      actualFilename = actualFilename + ".jpg"
      val outputFile: File = new File(filename);
      ImageIO.write(image, "jpg", outputFile);
    } catch {
      case _: IOException =>
        {
          Console.err.println("Error saving file.")
        }
    }
  }

  if (args.length > 0 && args(0) == "--export") {
    if (args.length < 2) {
      Console.err.println("Need to specify a filename when using --export.")
    }
    else {
      saveImage(rgbBitmap.getRenderedImage(), args(1))
    }
  }
  else {
    showImage(rgbBitmap.getImage())
  }

}
