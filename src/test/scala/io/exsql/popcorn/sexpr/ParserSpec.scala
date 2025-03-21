package io.exsql.popcorn.sexpr

import io.exsql.popcorn.sexpr.Parser.{AndExpr, Predicate, TraitExpr, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ParserSpec extends AnyFlatSpec with Matchers {
  "A sexpr.Parser" should "properly parse trait-eq expression" in {
    Parser.parse("""(trait-eq field "value")""") must be (
      TraitExpr(
        subject = "field",
        predicate = Predicate.TraitEq,
        `object` = Value("value")
      )
    )
  }

  it should "properly parse and expression" in {
    Parser.parse("""(and((trait-eq field "value1") (trait-eq field "value2")))""") must be(
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
