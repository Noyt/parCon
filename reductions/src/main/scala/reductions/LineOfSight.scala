package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    
    def fill(idx: Int, ang:Float, input: Array[Float], output: Array[Float]): Unit = {
      if(idx < input.length){
        val tmp = input(idx)/idx
        if(tmp > ang) {
          output(idx) = tmp
          fill(idx + 1, tmp, input, output)
        } else {
          output(idx) = ang
          fill(idx + 1, ang, input, output)
        }
      }
    }
    
    fill(1, 0, input, output)
    output(0) = 0
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    def rec(acc: Float, from: Int): Float = {
      if(from == until) acc
      else {
        val tmp = input(from)/from
        if(tmp > acc) rec(tmp, from + 1)
        else rec(acc, from + 1)
      }
    }
    rec(-1, from)
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if(end - from <= threshold){
      Leaf(from, end, upsweepSequential(input, from, end))
    } else {
      val mid = from + (end - from)/2
      val (l, r) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Node(l, r)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    def rec(from: Int, maxAngle: Float): Unit = {
      if(from < until){
        val m = max(input(from)/from, maxAngle)
        output(from) = m 
        rec(from + 1, m)
      }
    }
    rec(from, startingAngle)
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
    case Leaf(fr, un, maxP) => downsweepSequential(input, output, startingAngle, fr, un)
    case Node(l, r) => {
      parallel(
        downsweep(input, output, startingAngle, l),
        downsweep(input, output, max(startingAngle,l.maxPrevious), r)
        )
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val tree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0f, tree)
    output(0) = 0f;
  }
}
