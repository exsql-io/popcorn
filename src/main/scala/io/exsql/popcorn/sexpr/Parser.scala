package io.exsql.popcorn.sexpr

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import java.nio.charset.StandardCharsets

object Parser {
  def parse(sexpression: String): antlr4.sexpressionParser.SexprContext = {
    val lexer = new antlr4.sexpressionLexer(CharStreams.fromString(sexpression))
    val tokens = new CommonTokenStream(lexer)
    val parser = new antlr4.sexpressionParser(tokens)
    parser.sexpr()
  }
}
