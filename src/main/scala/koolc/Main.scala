package koolc

import utils._
import java.io.File

import lexer._
import ast._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil
    var tokens = false
    var ast = false
    var pretty = false
    var typed = false

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case "--tokens" :: args =>
        tokens = true
        processOption(args)

      case "--ast" :: args =>
        ast = true
        processOption(args)

      case "--pretty" :: args =>
        pretty = true
        processOption(args)

      case "--typed" :: args =>
        typed = true
        processOption(args)

      case f ::args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir, tokens = tokens, ast = ast, pretty = pretty, typed = typed)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    if (ctx.tokens) {
      val pipeline = Lexer andThen PrintTokens
      val program = pipeline.run(ctx)(ctx.file)

      // Consume the iterator
      program.length
    } else if (ctx.ast) {
      val pipeline = Lexer andThen Parser
      val program = pipeline.run(ctx)(ctx.file)

      println(Printer.ast(program))
    } else if (ctx.pretty) {
      //TODO: This is not a strict pretty
      val pipeline = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
      val program = pipeline.run(ctx)(ctx.file)

      println(Printer(program))
    } else if (ctx.typed) {
      val pipeline = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
      val program = pipeline.run(ctx)(ctx.file)

      println(Printer(program))

    } else {
      val pipeline = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
      pipeline.run(ctx)(ctx.file)
    }
  }
}
