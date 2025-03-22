package io.exsql.popcorn.engine

import io.exsql.popcorn.kernel.And
import io.exsql.popcorn.sexpr.Compiler.{AndExpr, SExpr, TraitExpr, Value}
import io.exsql.popcorn.{DataType, kernel}

import scala.reflect.ClassTag

object Engine {

  trait Batch[T] {
    def apply(ordinal: Int): Array[T]
    def zipWithIndex: Array[(Array[T], Int)]
  }

  final class ArrayBatch[T](vectors: Array[Array[T]]) extends Batch[T] {
    override def apply(ordinal: Int): Array[T] = vectors(ordinal)
    override def zipWithIndex: Array[(Array[T], Int)] = vectors.zipWithIndex
    override def toString: String = Engine.toString(vectors)
  }

  def evaluate[T](sexpr: SExpr, schema: String => (Int, DataType), batch: Batch[Any])
                 (implicit classTag: ClassTag[T]): Batch[T] = {

    new ArrayBatch[T](Array(
      sexpr match
        case AndExpr(left, right) =>
          val lresult = Engine.evaluate[Boolean](left, schema, batch)
          val rresult = Engine.evaluate[Boolean](right, schema, batch)
          And.and(lresult(0), rresult(0)).asInstanceOf[Array[T]]
        case TraitExpr(subject, _, value) =>
          val (ordinal, dataType) = schema(subject)
          evaluate[T](dataType, value.asInstanceOf[Value], batch(ordinal))
    ))
  }

  //BOOLEAN, LONG, DOUBLE, UTF8, BYTES
  private def evaluate[T](dataType: DataType, value: Value, vector: Array[Any]): Array[T] = {
    dataType match
      case DataType.BOOLEAN => ???
      case DataType.LONG => ???
      case DataType.DOUBLE => ???
      case DataType.UTF8 => vector.map(entry => kernel.Equals.equals(value.asUTF8, entry.asInstanceOf[Array[Byte]])).asInstanceOf[Array[T]]
  }

  private def toString[T](vectors: Array[Array[T]]): String = {
    vectors.map(Engine.toString).mkString("[", ",", "]")
  }

  private def toString[T](vector: Array[T]): String = {
    vector.map(_.toString).mkString("[", ",", "]")
  }

}