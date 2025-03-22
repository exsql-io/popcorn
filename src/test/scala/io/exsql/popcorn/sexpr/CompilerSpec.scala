package io.exsql.popcorn.sexpr

import io.exsql.popcorn.sexpr.Compiler.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must

class CompilerSpec extends AnyFlatSpec with must.Matchers {
  "A sexpr.Parser" should "properly compile eq function expression" in {
    Compiler.compile("""(eq field "value")""") must be (
      FnExpr(
        subject = "field",
        predicate = Predicate.Eq,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile ne function expression" in {
    Compiler.compile("""(ne field "value")""") must be(
      FnExpr(
        subject = "field",
        predicate = Predicate.Ne,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile gt function expression" in {
    Compiler.compile("""(gt field "value")""") must be(
      FnExpr(
        subject = "field",
        predicate = Predicate.Gt,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile gte function expression" in {
    Compiler.compile("""(gte field "value")""") must be(
      FnExpr(
        subject = "field",
        predicate = Predicate.Gte,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile lt function expression" in {
    Compiler.compile("""(lt field "value")""") must be(
      FnExpr(
        subject = "field",
        predicate = Predicate.Lt,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile lte function expression" in {
    Compiler.compile("""(lte field "value")""") must be(
      FnExpr(
        subject = "field",
        predicate = Predicate.Lte,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile in function expression" in {
    Compiler.compile("""(in field ("value1" "value2" "value3"))""") must be(
      FnExpr(
        subject = "field",
        predicate = Predicate.In,
        `object` = MultiValue(Array("value1", "value2", "value3"))
      )
    )
  }

  it should "properly compile and expression" in {
    Compiler.compile("""(and((eq field "value1") (eq field "value2")))""") must be(
      AndExpr(
        left = FnExpr(
          subject = "field",
          predicate = Predicate.Eq,
          `object` = Value("value1")
        ),
        right = FnExpr(
          subject = "field",
          predicate = Predicate.Eq,
          `object` = Value("value2")
        )
      )
    )
  }

  it should "properly compile or expression" in {
    Compiler.compile("""(or((eq field "value1") (eq field "value2")))""") must be(
      OrExpr(
        left = FnExpr(
          subject = "field",
          predicate = Predicate.Eq,
          `object` = Value("value1")
        ),
        right = FnExpr(
          subject = "field",
          predicate = Predicate.Eq,
          `object` = Value("value2")
        )
      )
    )
  }

  it should "properly compile not expression" in {
    Compiler.compile("""(not((eq field "value")))""") must be(
      NotExpr(
        child = FnExpr(
          subject = "field",
          predicate = Predicate.Eq,
          `object` = Value("value")
        )
      )
    )
  }
}
