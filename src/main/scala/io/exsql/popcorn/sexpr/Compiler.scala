package io.exsql.popcorn.sexpr

import java.nio.charset.StandardCharsets
import java.util
import scala.jdk.CollectionConverters.ListHasAsScala

object Compiler {

  /*  object Keywords {
      val True: String = "true"
      val False: String = "false"

      val Exists = "exists"
      val CiEquals = "ci-eq"
      val Contains = "contains"
      val CiContains = "ci-contains"
      val ElementContains = "element-contains"
      val Regex = "regex"
    }*/

  trait SExpr

  case class AndExpr(left: SExpr, right: SExpr) extends SExpr {
    override def toString: String = s"${Operators.and}($left $right)"
  }

  case class OrExpr(left: SExpr, right: SExpr) extends SExpr {
    override def toString: String = s"${Operators.or}($left $right)"
  }

  case class NotExpr(child: SExpr) extends SExpr {
    override def toString: String = s"${Operators.not}($child)"
  }

  case class FnExpr(subject: String, predicate: Predicate, `object`: SExpr) extends SExpr {
    override def toString: String = s"(${predicate.operation} $subject ${`object`})"
  }

  case class Value(text: String) extends SExpr {
    lazy val asUTF8: Array[Byte] = text.getBytes(StandardCharsets.UTF_8)
    lazy val asLong: Long = text.toLong
    lazy val asDouble: Double = text.toDouble
    lazy val asBoolean: Boolean = text.toBoolean

    override def toString: String = s""""$text""""
  }

  case class MultiValue(texts: Array[String]) extends SExpr {
    lazy val asUTF8: Array[Array[Byte]] = texts.map(_.getBytes(StandardCharsets.UTF_8))
    lazy val asLong: Array[Long] = texts.map(_.toLong)
    lazy val asDouble: Array[Double] = texts.map(_.toDouble)
    lazy val asBoolean: Array[Boolean] = texts.map(_.toBoolean)

    override def toString: String = {
      texts.map(text => s""""$text"""").mkString("(", " ", ")")
    }

    override def equals(obj: Any): Boolean = {
      if (!obj.isInstanceOf[MultiValue]) return false
      util.Arrays.equals(texts.asInstanceOf[Array[AnyRef]], obj.asInstanceOf[MultiValue].texts.asInstanceOf[Array[AnyRef]])
    }
  }

  private object Operators {
    val and = "and"
    val or = "or"
    val not = "not"
  }

  enum Predicate(val operation: String) {
    case Eq extends Predicate("eq")
    case Ne extends Predicate("ne")
    case Gt extends Predicate("gt")
    case Gte extends Predicate("gte")
    case Lt extends Predicate("lt")
    case Lte extends Predicate("lte")
    case In extends Predicate("in")
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
      case Operators.`or` =>
        val arguments = list.item(1).list_()
        OrExpr(left = compile(arguments.item(0)), right = compile(arguments.item(1)))
      case Operators.`not` =>
        val arguments = list.item(1).list_()
        NotExpr(child = compile(arguments.item(0)))
      case Predicate.Eq.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.Eq,
          `object` = compile(list.item(2))
        )
      case Predicate.Ne.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.Ne,
          `object` = compile(list.item(2))
        )
      case Predicate.Gt.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.Gt,
          `object` = compile(list.item(2))
        )
      case Predicate.Gte.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.Gte,
          `object` = compile(list.item(2))
        )
      case Predicate.Lt.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.Lt,
          `object` = compile(list.item(2))
        )
      case Predicate.Lte.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.Lte,
          `object` = compile(list.item(2))
        )
      case Predicate.In.`operation` =>
        FnExpr(
          subject = list.item(1).getText,
          predicate = Predicate.In,
          `object` = MultiValue(list.item(2).list_().item().asScala.map(item => unwrap(item.atom().getText)).toArray)
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
