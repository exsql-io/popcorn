package io.exsql.popcorn.sexpr

import io.exsql.popcorn.sexpr.Compiler.{AndExpr, Predicate, TraitExpr, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CompilerSpec extends AnyFlatSpec with Matchers {
  "A sexpr.Parser" should "properly compile trait-eq expression" in {
    Compiler.compile("""(trait-eq field "value")""") must be (
      TraitExpr(
        subject = "field",
        predicate = Predicate.TraitEq,
        `object` = Value("value")
      )
    )
  }

  it should "properly compile and expression" in {
    Compiler.compile("""(and((trait-eq field "value1") (trait-eq field "value2")))""") must be(
      AndExpr(
        left = TraitExpr(
          subject = "field",
          predicate = Predicate.TraitEq,
          `object` = Value("value1")
        ),
        right = TraitExpr(
          subject = "field",
          predicate = Predicate.TraitEq,
          `object` = Value("value2")
        )
      )
    )
  }
}
