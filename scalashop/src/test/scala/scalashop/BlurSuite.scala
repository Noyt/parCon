package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test def `le test`: Unit = {
    assert(1 == 1)
  }
}

