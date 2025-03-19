package io.exsql.popcorn

import com.google.common.base.Stopwatch
import com.google.common.hash.Hashing
import io.exsql.popcorn.kernel.{Equals, Sum}
import net.datafaker.Faker

import java.nio.charset.StandardCharsets
import java.util
import java.util.concurrent.TimeUnit

object Bootstrap {

  @main def run(): Unit = {
    //sum()
    stringEquals()
  }

  private def sum(): Unit = {
    //    val _100mi = Array.range(0, 100000000)
    //    println(s"int dataset length: ${_100mi.length}")

    val _100ml = Array.range(0, 100000000).map(_.toLong)
    println(s"long dataset length: ${_100ml.length}")

    //    val _100md = Array.range(0, 100000000).map(_.toDouble)
    //    println(s"double dataset length: ${_100md.length}")

    //    (0 to 10).foreach { run =>
    //      withTiming(s"sum(int) for run#$run") {
    //        Sum.sum(_100mi, _100mi)
    //      }
    //    }

    (0 to 10).foreach { run =>
      withTiming(s"Sum.ofLong for run#$run") {
        Sum.ofLong(_100ml, _100ml)
      }
    }

    (0 to 10).foreach { run =>
      val left = util.Arrays.copyOf(_100ml, _100ml.length)
      val right = util.Arrays.copyOf(_100ml, _100ml.length)

      withTiming(s"Sum.ofLongUnsafe for run#$run") {
        Sum.ofLongUnsafe(left, right)
      }
    }

    //    (0 to 10).foreach { run =>
    //      withTiming(s"sum(double) for run#$run") {
    //        Sum.sum(_100md, _100md)
    //      }
    //    }
  }

  private def stringEquals(): Unit = {
    val faker = new Faker()
    var name1 = faker.naruto().character().getBytes(StandardCharsets.UTF_8)
    var name2 = faker.naruto().character().getBytes(StandardCharsets.UTF_8)
    if (name1.length != name2.length) {
      if (name1.length < name2.length) {
        name1 = util.Arrays.copyOf(name1, name2.length)
      }

      if (name2.length < name1.length) {
        name2 = util.Arrays.copyOf(name2, name1.length)
      }
    }

    println(s"${new String(name1)} vs ${new String(name2)}")
    val hash1 = Hashing.md5().hashBytes(name1).asBytes()
    val hash2 = Hashing.md5().hashBytes(name2).asBytes()

    (0 to 10).foreach { run =>
      withTiming(s"Equals.equals for run#$run") {
        (0 until 1_000_000).map { _ =>
          Equals.equals(hash1, hash2)
        }
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
