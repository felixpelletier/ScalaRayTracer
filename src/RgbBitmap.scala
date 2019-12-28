import java.awt.{Color, Image}
import java.awt.image.BufferedImage

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
