package gradient.kernel

object sobel {
  def yOfSize(size: Int): Kernel = {
    val center = size / 2
    val matrix = Vector.tabulate(size, size) { (y, x) =>
      if (x == center && y == center) 0
      else {
        (y - center) / (Math.pow(y - center, 2) + Math.pow(x - center, 2))
      }
    }
    KernelWithMatrix(matrix, size)
  }

  def xOfSize(size: Int): Kernel = {
    val center = size / 2
    val matrix = Vector.tabulate(size, size) { (y, x) =>
      if (x == center && y == center) 0
      else {
        (x - center) / (Math.pow(y - center, 2) + Math.pow(x - center, 2))
      }
    }
    KernelWithMatrix(matrix, size)
  }
}