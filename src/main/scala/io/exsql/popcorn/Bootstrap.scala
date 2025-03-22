package io.exsql.popcorn

import com.google.common.base.Stopwatch
import com.typesafe.scalalogging.StrictLogging
import io.exsql.popcorn.engine.Engine
import io.exsql.popcorn.engine.Engine.{ArrayBatch, Batch}
import io.exsql.popcorn.sexpr.Compiler
import io.exsql.popcorn.sexpr.Compiler.*
import net.datafaker.Faker

import java.nio.charset.StandardCharsets
import java.util
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.boundary

object Bootstrap extends StrictLogging {

  @main def run(sexprCount: Int, batchCount: Int, batchSize: Int, validate: Boolean): Unit = {
    val executor = Executors.newFixedThreadPool(4)

    val faker = new Faker()
    val fields = Array(DataType.UTF8, DataType.UTF8)
    val sexprs = generate(faker, sexprCount)
    (0 until batchCount).foreach { id =>
      if (validate) executor.execute(() => { runAndValidate(id, faker, sexprs, sexprCount, batchSize, fields) })
      else executor.execute(() => { run(id, faker, sexprs, sexprCount, batchSize, fields) })
    }

    executor.shutdown()
  }

  private def run(id: Int, faker: Faker, sexprs: Array[SExpr], sexprCount: Int, batchSize: Int, fields: Array[DataType]): Unit = {
    val batch = generate[Array[Byte]](faker, batchSize, fields).asInstanceOf[Batch[Any]]
    withTiming(s"[batch #$id] - evaluate $sexprCount s-expressions on $batchSize values") {
      sexprs.foreach { sexpr =>
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

  private def runAndValidate(id: Int, faker: Faker, sexprs: Array[SExpr], sexprCount: Int, batchSize: Int, fields: Array[DataType]): Unit = {
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
          left.asInstanceOf[MultiValue].asUTF8.foreach { value =>
            validate[Array[Byte]](batch.asInstanceOf[Batch[Array[Byte]]], results(index)(0), Array(value, right.asInstanceOf[Value].asUTF8))
          }
    }
  }

  private def generate(faker: Faker, size: Int): Array[SExpr] = {
    Array.fill[SExpr](size)(Compiler.compile(s"""(and((in field1 ("${faker.naruto().character()}" "${faker.naruto().character()}" "${faker.naruto().character()}")) (eq field2 "${faker.naruto().character()}")))"""))
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

  private def validate[T](dataset: Batch[T], results: Array[Boolean], values: Array[T]): Boolean = {
    boundary:
      results.zipWithIndex.foreach { case (result, ordinal) =>
        if (result) {
          values.zipWithIndex.foreach { case (value, index) =>
            boundary.break(util.Arrays.equals(dataset(index)(ordinal).asInstanceOf[Array[Byte]], value.asInstanceOf[Array[Byte]]))
          }
        }
      }

    true
  }

}
