package io.exsql.popcorn

import com.google.common.base.Stopwatch
import com.typesafe.scalalogging.StrictLogging
import io.exsql.popcorn.engine.Engine
import io.exsql.popcorn.engine.Engine.{ArrayBatch, Batch}
import io.exsql.popcorn.sexpr.Compiler
import io.exsql.popcorn.sexpr.Compiler.{AndExpr, SExpr, FnExpr, Value}
import net.datafaker.Faker

import java.nio.charset.StandardCharsets
import java.util
import java.util.concurrent.{Executors, TimeUnit}

object Bootstrap extends StrictLogging {

  @main def run(sexprCount: Int, batchCount: Int, batchSize: Int): Unit = {
    val executor = Executors.newFixedThreadPool(2)

    val faker = new Faker()
    val fields = Array(DataType.UTF8, DataType.UTF8)
    val sexprs = generate(faker, sexprCount)
    (0 until batchCount).foreach { id =>
      executor.execute(
        () => {
          val batch = generate[Array[Byte]](faker, batchSize, fields).asInstanceOf[Batch[Any]]
          val results = {
            withTiming(s"[batch #$id] - evaluate $sexprCount s-expressions on $batchSize values") {
              sexprs.map { sexpr =>
                Engine.evaluate[Boolean](
                  sexpr = sexpr,
                  schema = {
                    case "field1" => 0 -> fields(0)
                    case "field2" => 1 -> fields(1)
                  },
                  batch = batch
                )
              }
            }
          }

          sexprs.zipWithIndex.foreach { case (sexpr, index) =>
            logger.debug(s"validating s-expression: $sexpr")
            sexpr match
              case FnExpr(_, _, value) => validate[Array[Byte]](batch.asInstanceOf[Batch[Array[Byte]]], results(index)(0), Array(value.asInstanceOf[Value].asUTF8))
              case AndExpr(FnExpr(_, _, left), FnExpr(_, _, right)) =>
                validate[Array[Byte]](batch.asInstanceOf[Batch[Array[Byte]]], results(index)(0), Array(left.asInstanceOf[Value].asUTF8, right.asInstanceOf[Value].asUTF8))
          }
        }
      )
    }

    executor.shutdown()
  }

  private def generate(faker: Faker, size: Int): Array[SExpr] = {
    Array.fill[SExpr](size)(Compiler.compile(s"""(and((eq field1 "${faker.naruto().character()}") (eq field2 "${faker.naruto().character()}")))"""))
  }

  private def generate[T](faker: Faker, size: Int, fields: Array[DataType]): Batch[T] = {
    new ArrayBatch(
      fields
        .map {
          case DataType.BOOLEAN => ???
          case DataType.LONG => ???
          case DataType.DOUBLE => ???
          case DataType.UTF8 => strings(faker, size)
        }
        .asInstanceOf[Array[Array[T]]]
    )
  }

  private def strings(faker: Faker, size: Int): Array[Array[Byte]] = {
    Array.fill[Array[Byte]](size)(faker.naruto().character().getBytes(StandardCharsets.UTF_8))
  }

  private def withTiming[T](label: String)(code: => T): T = {
    val watch = Stopwatch.createStarted()
    val result = code
    watch.stop()
    println(s"$label took: ${watch.elapsed(TimeUnit.MILLISECONDS)} ms")
    result
  }

  private def validate[T](dataset: Batch[T], results: Array[Boolean], values: Array[T]): Unit = {
    results.zipWithIndex.foreach { case (result, ordinal) =>
      if (result) {
        values.zipWithIndex.foreach { case (value, index) =>
          if (result) {
            require(util.Arrays.equals(dataset(index)(ordinal).asInstanceOf[Array[Byte]], value.asInstanceOf[Array[Byte]]))
          }
        }
      }
    }
  }

}
