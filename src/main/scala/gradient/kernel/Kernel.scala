package gradient.kernel

import java.awt.Color
import java.awt.image.BufferedImage

import gradient.syntax.*

trait Kernel {
  def apply(image: BufferedImage, x: Int, y: Int, channel: Color => Int): Int
  def center: Int
}

class KernelWithMatrix(matrix: Vector[Vector[Double]], size: Int) extends Kernel {
  private val factor = matrix.flatten.filter{ _ > 0 }.sum

  def center: Int = size / 2

  def apply(image: BufferedImage, x: Int, y: Int, channel: Color => Int): Int = {
    (LazyList
      .tabulate(size, size){ (j, i) =>
        channel(image.getColor(x + i - center, y + j - center)) * matrix(j)(i)
      }.flatten.sum / factor).round.toInt
  }

  override def toString: String =
    matrix
      .map(row => row.map(x => "%05.2f".format(x)).mkString(" "))
      .mkString("\n")
}
