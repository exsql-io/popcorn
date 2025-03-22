package io.exsql.popcorn.engine

import io.exsql.popcorn.kernel.{And, Not, Or}
import io.exsql.popcorn.sexpr.Compiler.*
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
          val lr = Engine.evaluate[Boolean](left, schema, batch)
          val rr = Engine.evaluate[Boolean](right, schema, batch)
          And.and(lr(0), rr(0)).asInstanceOf[Array[T]]
        case OrExpr(left, right) =>
          val lr = Engine.evaluate[Boolean](left, schema, batch)
          val rr = Engine.evaluate[Boolean](right, schema, batch)
          Or.or(lr(0), rr(0)).asInstanceOf[Array[T]]
        case NotExpr(child) =>
          val result = Engine.evaluate[Boolean](child, schema, batch)
          Not.not(result(0)).asInstanceOf[Array[T]]
        case FnExpr(subject, predicate, value) =>
          val (ordinal, dataType) = schema(subject)
          evaluate[T](predicate, dataType, value.asInstanceOf[Value], batch(ordinal))
    ))
  }

  private def evaluate[T](predicate: Predicate, dataType: DataType, value: Value, vector: Array[Any]): Array[T] = {
    val result = {
      dataType match {
        case DataType.BOOLEAN => ???
        case DataType.LONG => ???
        case DataType.DOUBLE => ???
        case DataType.UTF8 =>
          predicate match
            case Predicate.Eq => vector.map(entry => kernel.Ordering.equal(value.asUTF8, entry.asInstanceOf[Array[Byte]]))
            case Predicate.Ne => vector.map(entry => kernel.Ordering.notEqual(value.asUTF8, entry.asInstanceOf[Array[Byte]]))
            case Predicate.Gt => vector.map(entry => kernel.Ordering.greaterThan(value.asUTF8, entry.asInstanceOf[Array[Byte]]))
            case Predicate.Gte => vector.map(entry => kernel.Ordering.greaterThanOrEqual(value.asUTF8, entry.asInstanceOf[Array[Byte]]))
            case Predicate.Lt => vector.map(entry => kernel.Ordering.lessThan(value.asUTF8, entry.asInstanceOf[Array[Byte]]))
            case Predicate.Lte => vector.map(entry => kernel.Ordering.lessThanOrEqual(value.asUTF8, entry.asInstanceOf[Array[Byte]]))
      }
    }

    result.asInstanceOf[Array[T]]
  }

  private def toString[T](vectors: Array[Array[T]]): String = {
    vectors.map(Engine.toString).mkString("[", ",", "]")
  }

  private def toString[T](vector: Array[T]): String = {
    vector.map(_.toString).mkString("[", ",", "]")
  }

}