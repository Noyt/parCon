import java.util.concurrent._
import scala.util.DynamicVariable

import org.scalameter._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    /* var r = 0
    var g = 0
    var b = 0
    var a = 0
    
    val dim = (1+2*radius)
    val nbPixels = dim*dim
    
    val startX = x - radius
    val startY = y - radius
    val endX = x + radius
    val endY = x + radius

    var currX = startX
    var currY = startY
    


    // TODO check bound
    while(currY <= endY){
      while(currX <= endX){
        var tmp = src apply (clamp(currX, 0, src.width-1), clamp(currY, 0, src.height-1))
        r = r + red(tmp)
        g = g + green(tmp)
        b = b + blue(tmp)
        a = a + alpha(tmp)

        currX = currX + 1
      }
      currX = startX
      currY = currY + 1
    }
    
    return rgba(r/nbPixels, g/nbPixels, b/nbPixels, a/nbPixels) */

    val xMin = clamp(x - radius, 0, src.width - 1)
    val xMax = clamp(x + radius, 0, src.width - 1)

    val yMin = clamp(y - radius, 0, src.height - 1)
    val yMax = clamp(y + radius, 0, src.height - 1)

    var xr = xMin
    var r, g, b, a, count = 0

    while (xr <= xMax) {
      var yr = yMin
      while (yr <= yMax) {
        val pix = src(xr, yr)

        r = r + red(pix)
        g = g + green(pix)
        b = b + blue(pix)
        a = a + alpha(pix)

        yr = yr + 1
        count = count + 1
      }
      xr = xr + 1
    }

    rgba(r / count, g / count, b / count, a / count)
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  // Workaround Dotty's handling of the existential type KeyValue
  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}
