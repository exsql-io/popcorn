package io.exsql.popcorn.sexpr

import java.nio.charset.StandardCharsets

object Compiler {

  /*  object Keywords {
      val True: String = "true"
      val False: String = "false"
      // Operators.
      val Or: String = "or"
      val Not: String = "not"
      // Core type predicates.
      val IdType = "id-type"
      val Source = "source"
      // Compound expressions.
      val EventHistory = "event-history"
      // Event property predicates.
      val PropertyEq = "property-eq"
      val PropertyNe = "property-ne"
      val PropertyCiEquals = "property-ci-eq"
      val PropertyContains = "property-contains"
      val PropertyCiContains = "property-ci-contains"
      val PropertyRegex = "property-regex"
      val PropertyExists = "property-exists"
      val PropertyGte = "property-gte"
      val PropertyGt = "property-gt"
      val PropertyLte = "property-lte"
      val PropertyLt = "property-lt"
      val PropertyIn = "property-in"

      // Trait predicates.
      val TraitNe = "trait-ne"
      val TraitExists = "trait-exists"
      val TraitCiEquals = "trait-ci-eq"
      val TraitContains = "trait-contains"
      val TraitCiContains = "trait-ci-contains"
      val TraitElementContains = "trait-element-contains"
      val TraitRegex = "trait-regex"
      val TraitGte = "trait-gte"
      val TraitGt = "trait-gt"
      val TraitLte = "trait-lte"
      val TraitLt = "trait-lt"
      val TraitIn = "trait-in"
    }*/

  trait SExpr

  case class AndExpr(left: SExpr, right: SExpr) extends SExpr {
    override def toString: String = s"${Operators.and}($left $right)"
  }

  case class TraitExpr(subject: String, predicate: Predicate, `object`: SExpr) extends SExpr {
    override def toString: String = s"(${predicate.operation} $subject ${`object`})"
  }

  case class Value(text: String) extends SExpr {
    lazy val asUTF8: Array[Byte] = text.getBytes(StandardCharsets.UTF_8)
    lazy val asLong: Long = text.toLong
    lazy val asDouble: Double = text.toDouble
    lazy val asBoolean: Boolean = text.toBoolean

    override def toString: String = s""""$text""""
  }

  object Operators {
    val and = "and"
  }

  enum Predicate(val operation: String) {
    case TraitEq extends Predicate("trait-eq")
  }

  def compile(sexpression: String): SExpr = {
    compile(Parser.parse(sexpression))
  }

  private def compile(sexpr: antlr4.sexpressionParser.SexprContext): SExpr = {
    val items = sexpr.item()
    val item = items.getFirst
    if (item.atom() != null) return compile(item)
    compile(item.list_())
  }

  private def compile(list: antlr4.sexpressionParser.List_Context): SExpr = {
    val item = list.item(0)
    if (item.list_() != null) return compile(item.list_())

    item.getText match
      case Operators.`and` =>
        val arguments = list.item(1).list_()
        AndExpr(left = compile(arguments.item(0)), right = compile(arguments.item(1)))
      case Predicate.TraitEq.`operation` =>
        TraitExpr(
          subject = list.item(1).getText,
          predicate = Predicate.TraitEq,
          `object` = compile(list.item(2))
        )
  }

  private def compile(item: antlr4.sexpressionParser.ItemContext): SExpr = {
    if (item.atom() != null) Value(unwrap(item.atom().getText))
    else compile(item.list_())
  }

  private def unwrap(text: String): String = {
    text.substring(1, text.length - 1)
  }

}
