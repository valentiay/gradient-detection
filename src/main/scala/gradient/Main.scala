package gradient

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import gradient.syntax.*
import gradient.kernel.*

def convolutionXY(image: BufferedImage, kernelX: Kernel, kernelY: Kernel): BufferedImage = {
  val convolutedImage = BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_ARGB)
  val margin = kernelX.center.max(kernelY.center)
  for {
    x <- image.xIndices.drop(margin).dropRight(margin)
    y <- image.yIndices.drop(margin).dropRight(margin)
  } {
    val red = Math.sqrt(Math.pow(kernelX.apply(image, x, y, _.getRed), 2) + Math.pow(kernelY.apply(image, x, y, _.getRed), 2)).ceil.toInt.min(255)
    val green = Math.sqrt(Math.pow(kernelX.apply(image, x, y, _.getGreen), 2) + Math.pow(kernelY.apply(image, x, y, _.getGreen), 2)).ceil.toInt.min(255)
    val blue = Math.sqrt(Math.pow(kernelX.apply(image, x, y, _.getBlue), 2) + Math.pow(kernelY.apply(image, x, y, _.getBlue), 2)).ceil.toInt.min(255)
    convolutedImage.setRGB(x, y, Color(red, green, blue).getRGB)
  }
  convolutedImage
}

def equalsApprox(color1: Color, color2: Color): Boolean =
  (color1.getRed - color2.getRed).abs <= maxDiff &&
    (color1.getGreen - color2.getGreen).abs <= maxDiff &&
    (color1.getBlue - color2.getBlue).abs <= maxDiff

def findAreas(convolution: BufferedImage): BufferedImage = {
  val areas = BufferedImage(convolution.getWidth, convolution.getHeight, BufferedImage.TYPE_INT_ARGB)
  for {
    y <- convolution.yIndices.tail
    x <- convolution.xIndices.tail
  } {
    val upperColor = convolution.getColor(x, y - 1)
    val leftColor = convolution.getColor(x - 1, y)
    val currentColor = convolution.getColor(x, y)
    if ((equalsApprox(upperColor, currentColor) || equalsApprox(leftColor, currentColor)) && currentColor != Color(0, 0, 0) ) {
      areas.setRGB(x, y, convolution.getRGB(x, y))
    } else {
      areas.setRGB(x, y, Color(0, 0, 0, 0).getRGB)
    }
  }
  areas
}

def copyRectangle(source: BufferedImage, target: BufferedImage, startX: Int, endX: Int, startY: Int, endY: Int): Unit =
  for {
    x <- startX.until(endX)
    y <- startY.until(endY)
  } target.setRGB(x, y, source.getRGB(x, y))

def findRectangles(image: BufferedImage, areas: BufferedImage): BufferedImage = {
  val firstRow = Vector.tabulate(areas.getWidth)(x => (x, 0))
  val matrix = areas.yIndices.tail.foldLeft(Vector(firstRow)){(rows, y) =>
    val first =
      if (areas.getColor(0, y - 1).getAlpha > 0 && areas.getColor(0, y).getAlpha > 0) rows(y - 1)(0)
      else (0, y)
    val newRow =
      areas.xIndices.tail.foldLeft(Vector(first)){ (row, x) =>
        val upperColor = areas.getColor(x, y - 1)
        val leftColor = areas.getColor(x - 1, y)
        val newElement =
          if (areas.getColor(x, y).getAlpha == 0) (x, y)
          else if (areas.getColor(x - 1, y).getAlpha > 0) row(x - 1)
          else if (areas.getColor(x, y - 1).getAlpha > 0) rows(y - 1)(x)
          else (x, y)
        row.appended(newElement)
      }
    rows.appended(newRow)
  }

  val rectangles = BufferedImage(areas.getWidth, areas.getHeight, BufferedImage.TYPE_INT_ARGB)
  for {
    y <- rectangles.yIndices.reverse
    x <- rectangles.xIndices.reverse
  } rectangles.setRGB(x, y, Color(0, 0, 0, 0).getRGB)
  for {
    y <- areas.yIndices.reverse
    x <- areas.xIndices.reverse
  } {
    if (rectangles.getColor(x, y).getAlpha == 0 && matrix(y)(x) != (x, y)) {
      val (startX, startY) = matrix(y)(x)
      if (x - startX >= minRectSide && y - startY >= minRectSide) {
        copyRectangle(image, rectangles, startX, x, startY, y)
      } else ()
    } else ()
  }
  rectangles
}

def printImage(image: BufferedImage, channel: Color => Int): Unit = {
  for {
    y <- (image.getMinY until (image.getMinY + image.getHeight)).take(10)
  } {
    for {
      x <- image.getMinX until (image.getMinX + image.getWidth)
    } print("%03d".format(image.getColor(x, y).getBlue) + " ")
    println()
  }
}


val maxDiff = 1
val minRectSide = 5
val gradientKernelSize = 3

@main def main(inputFile: String, otherArgs: String*): Unit = {
  val imgPath = new File(inputFile)
  val image   = ImageIO.read(imgPath)

  val raster     = image.getRaster
  val convoluted = convolutionXY(image, sobel.xOfSize(gradientKernelSize), sobel.yOfSize(gradientKernelSize))
  val areas      = findAreas(convoluted)
  val rectangles = findRectangles(image, areas)

  val outputfile = new File(otherArgs.headOption.getOrElse("output.png"))
  ImageIO.write(rectangles, "png", outputfile)
}
