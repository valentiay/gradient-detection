package gradient

import java.awt.image.BufferedImage
import java.awt.Color

object syntax {
  extension (image: BufferedImage)
    def getColor(x: Int, y: Int): Color = Color(image.getRGB(x, y), true)
    def xIndices: Range = image.getMinX.until(image.getMinX + image.getWidth)
    def yIndices: Range = image.getMinY.until(image.getMinY + image.getHeight)
}
