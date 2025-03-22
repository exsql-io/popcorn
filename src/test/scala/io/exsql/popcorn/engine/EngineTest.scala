package io.exsql.popcorn.engine

import io.exsql.popcorn.DataType
import io.exsql.popcorn.engine.Engine.ArrayBatch
import io.exsql.popcorn.sexpr.Compiler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must

import java.nio.charset.StandardCharsets
import scala.reflect.ClassTag

class EngineTest extends AnyFlatSpec with must.Matchers {

  private val fieldString: String => (Int, DataType) = {
    case "field" => (0, DataType.UTF8)
  }

  private val tupleFieldString: String => (Int, DataType) = {
    case "field1" => (0, DataType.UTF8)
    case "field2" => (1, DataType.UTF8)
  }

  "An Engine" should """properly evaluate (eq field "value")""" in {
    evaluate[Array[Byte], Boolean](
      """(eq field "value")""",
      fieldString,
      newUTF8Input(Array(Array("value")))
    ).toString must be ("[[true]]")
  }

  it should """properly evaluate (ne field "value")""" in {
    evaluate[Array[Byte], Boolean](
      """(ne field "value")""",
      fieldString,
      newUTF8Input(Array(Array("value")))
    ).toString must be("[[false]]")
  }

  it should """properly evaluate (gt field "value")""" in {
    evaluate[Array[Byte], Boolean](
      """(gt field "value")""",
      fieldString,
      newUTF8Input(Array(Array("value")))
    ).toString must be("[[false]]")
  }

  it should """properly evaluate (gte field "value")""" in {
    evaluate[Array[Byte], Boolean](
      """(gte field "value")""",
      fieldString,
      newUTF8Input(Array(Array("value")))
    ).toString must be("[[true]]")
  }

  it should """properly evaluate (lt field "value")""" in {
    evaluate[Array[Byte], Boolean](
      """(lt field "value")""",
      fieldString,
      newUTF8Input(Array(Array("value")))
    ).toString must be("[[false]]")
  }

  it should """properly evaluate (lte field "value")""" in {
    evaluate[Array[Byte], Boolean](
      """(lte field "value")""",
      fieldString,
      newUTF8Input(Array(Array("value")))
    ).toString must be("[[true]]")
  }

  it should """properly evaluate (in field ("value1" "value2" "value3"))""" in {
    evaluate[Array[Byte], Boolean](
      """(in field ("value1" "value2" "value3"))""",
      fieldString,
      newUTF8Input(Array(Array("value2")))
    ).toString must be("[[true]]")
  }

  it should """properly evaluate (and((eq field1 "value1") (eq field2 "value2")))""" in {
    evaluate[Array[Byte], Boolean](
      """(and((eq field1 "value1") (eq field2 "value2")))""",
      tupleFieldString,
      newUTF8Input(Array(Array("value1"), Array("value2")))
    ).toString must be ("[[true]]")
  }

  it should """properly evaluate (or((eq field "value1") (eq field "value2")))""" in {
    evaluate[Array[Byte], Boolean](
      """(or((eq field "value1") (eq field "value2")))""",
      fieldString,
      newUTF8Input(Array(Array("value1", "value2", "value3")))
    ).toString must be("[[true,true,false]]")
  }

  it should """properly evaluate (not((eq field "value")))""" in {
    evaluate[Array[Byte], Boolean](
      """(not((eq field "value")))""",
      fieldString,
      newUTF8Input(Array(Array("value1", "value", "value3")))
    ).toString must be("[[true,false,true]]")
  }

  private def evaluate[Input, Output](sexpression: String, schema: String => (Int, DataType), input: Engine.Batch[Input])
                                     (implicit classTag: ClassTag[Output]): Engine.Batch[Output] = {

    Engine.evaluate(Compiler.compile(sexpression), schema, input.asInstanceOf[Engine.Batch[Any]])
  }

  private def newUTF8Input(values: Array[Array[String]]): Engine.Batch[Array[Byte]] = {
    new ArrayBatch[Array[Byte]](values.map(vector => vector.map(_.getBytes(StandardCharsets.UTF_8))))
  }

}
