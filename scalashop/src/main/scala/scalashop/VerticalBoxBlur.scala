package scalashop

import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 2
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 100
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for {
      x <- from until end
      y <- 0 until src.height
    } yield dst.update(x,y,boxBlurKernel(src,x,y,radius)) 
  }
  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    
    val points = 0 to src.width by (src.width/numTasks max 1)
    val tuples = points zip (points tail)
    
    tuples.map{
      case (start, end) => task(blur(src, dst, start, end, radius))
    }.foreach(_.join)
  }
}
