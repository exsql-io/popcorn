package io.exsql.popcorn

import com.google.common.base.Stopwatch
import io.exsql.popcorn.kernel.Kernel

import java.util.concurrent.TimeUnit

object Bootstrap {

  @main def run(): Unit = {
    val _100m = Array.range(0, 100000000)
    println(s"int dataset length: ${_100m.length}")
    (0 to 10).foreach { run =>
      withTiming(s"sum for run#$run") {
        Kernel.sum(_100m, _100m)
      }
    }
  }

  private def withTiming(label: String)(code: => Unit): Unit = {
    val watch = Stopwatch.createStarted()
    code
    watch.stop()
    println(s"$label took: ${watch.elapsed(TimeUnit.MILLISECONDS)} ms")
  }

}
