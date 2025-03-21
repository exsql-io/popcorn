package io.exsql.popcorn.sexpr

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import java.nio.charset.StandardCharsets

object Parser {

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

  def parse(sexpression: String): SExpr = {
    val lexer = new antlr4.sexpressionLexer(CharStreams.fromString(sexpression))
    val tokens = new CommonTokenStream(lexer)
    val parser = new antlr4.sexpressionParser(tokens)
    compile(parser.sexpr())
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
