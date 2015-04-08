package koolc
package combinators

import utils._
import scala.collection.immutable.Stream

/*
 * Every parser can have a name which is used in error reporting.
 */
trait Named {
  val name: String = "Unnamed"
}

/*
 * In general, a parser is function mapping some Stream of type a to a result
 * of type b. In the case of the Lexer, this means a Parser maps a Stream of
 * chars to some token.
 */
abstract class ParserCombinator[a, b] extends Named {
  // See below for some reasoning on the types and on error handling.
  private type PositiveResult = ParserCombinator.PositiveResult[a, b]
  private type NegativeResult = ParserCombinator.NegativeResult
  private type ParserResult = ParserCombinator.ParserResult[a, b]
  def apply(in: Stream[a]): ParserResult

  /*
   * This function chooses the 'better' negative result of two alternatives.
   * If there are two negative results, we assume the longer is the one which
   * comes closer to the author's original intention. Since the length of some
   * positive result of the left hand side might influence the length of the
   * right hand side, specifically if we chain two parsers, we need another
   * parameter li.
   */
  private def buildNegativeFromTwo(lneg: Option[NegativeResult], li: Int, rneg: Option[NegativeResult]): Option[NegativeResult] = {
    (lneg, rneg) match {
      case (None, None) => None
      case (None, Some((rnms, rj))) => Some((rnms, li + rj))
      case (l@Some(_), None) => l
      case (Some((_, lj)), Some ((_, rj))) => {
        if (lj > li + rj)
          lneg
        else if (lj < li + rj) {
          val Some((rnms, rj)) = rneg

          Some((rnms, li + rj))
        } else {
          val (Some((lnms, _)), Some((rnms, _))) = (lneg, rneg)

          Some((lnms union rnms, lj))
        }
      }
    }
  }

  /*
   * A parser can be combined with another one to form a new parser. The two
   * main combinations are chaining and or combinations which are both
   * implemented here. Using these we can derive many helpful additional
   * parsers like a Kleene star.
   */

  /*
   * Two parsers can be or-combined if they are of the same type [a, b]. In
   * this implementation, we always have to try both parsers since the right
   * one might produce a better error than the left one (despite a positive
   * match). Since this might be very expensive it might be a good idea to make
   * this optional only when good errors are actually desired.
   *
   * If both parsers match, this general combinator uses some function to choose a positive result.
   * Below
   */
  private type OrChooser = (PositiveResult, PositiveResult) => PositiveResult
  private def ||?(chooser: OrChooser)(rhs: => ParserCombinator[a, b]): ParserCombinator[a, b] =
    ParserCombinator.combinator {
      lazy val r = rhs
      xs: Stream[a] => {
        val (lpos, lneg) = this(xs)
        val (rpos, rneg) = r(xs)

        val pos = (lpos, rpos) match {
          case (None, rpos) => rpos
          case (lpos, None) => lpos
          case (Some(lp), Some(rp)) => Some(chooser(lp, rp))
        }
        val neg = buildNegativeFromTwo(lneg, 0, rneg)
        (pos, neg)
      }
    }

  /*
   * Prefer lhs over rhs.
   * Note that if we did not care about errors we could make this one short circuited.
   */
  val || = ||?( (l, _) => l ) _

  /*
   * Choose the longer of the two matches.
   */
  val ||> = {
    def chooseLonger(l: PositiveResult, r: PositiveResult) = {
      val (_, _, li) = l
      val (_, _, ri) = r

      if (ri > li)
        r
      else
        l
    }

    ||?(chooseLonger) _
  }

  /*
   * We can chain two parsers of the same input type [a]. We will only try the
   * second parser if the first one produced a match, finally resulting in a
   * tuple of results (lhs, rhs).
   */
  def **[c](rhs: => ParserCombinator[a, c]): ParserCombinator[a, (b, c)] = ParserCombinator.combinator {
    lazy val r = rhs
    xs: Stream[a] => this(xs) match {
      // If lhs does not match, we're done.
      case (None, lneg) => (None, lneg)
      // If lhs matches...
      case (Some((lb, lys, li)), lneg) => {
        r(lys) match {
          // ... and rhs does not, use the longer error
          case (None, rneg) => {
            (None, buildNegativeFromTwo(lneg, li, rneg))
          }
          // ... and rhs does, too, create a positive result and use the longer error
          case (Some((rb, rys, ri)), rneg) => {
            val pos = Some(((lb, rb), rys, li + ri))
            val neg = buildNegativeFromTwo(lneg, li, rneg)

            (pos, neg)
          }
        }
      }
    }
  }

  /*
   * Chain two parsers and ignore the left result.
   */
  def *>[c](rhs: => ParserCombinator[a, c]): ParserCombinator[a, c] =
    this ** rhs >> { case (_, r) => r }

  /*
   * Chain two parsers and ignore the right result.
   */
  def *<[c](rhs: => ParserCombinator[a, c]): ParserCombinator[a, b] =
    this ** rhs >> { case (l, _) => l }

  /*
   * This applies a parser and if successful transforms the result using the
   * function f.
   */
  def >>[c](f: b => c): ParserCombinator[a, c] = {
    val outer= this

    new ParserCombinator[a, c] {
      override val name = outer.name

      def apply(xs: Stream[a]) = outer(xs) match {
        case (Some((b, ys, i)), neg) => (Some((f(b), ys, i)), neg)
        case (None, neg) => (None, neg)
      }
    }
  }
}

object ParserCombinator {
  type PositiveResult[a, b] = (b, Stream[a], Int)
  type NegativeResult = (Set[Named], Int)
  type ParserResult[a, b] = (Option[PositiveResult[a, b]], Option[NegativeResult])
  type ApplyType[a, b] = Stream[a] => ParserResult[a, b]
  private type PC[a, b] = ParserCombinator[a, b]

  def named[a, b](nme: String)(p: PC[a, b]): PC[a, b] =
    new PC[a, b] {
      override val name = nme
      def apply(xs: Stream[a]) = p(xs) match {
        case (pos, Some((nms, i))) => (pos, Some((Set(this), i)))
        case nonNegative => nonNegative
      }
    }

  /*
   * Nicer way to define a new combinator
   */
  def combinator[a, b](aply: ApplyType[a, b]) =
    new PC[a, b] {
      def apply(xs: Stream[a]) = aply(xs)
    }

  def fuse[t] = ((x: t, xs: List[t]) => x +: xs).tupled

  /*
   * A combinator that does not consume anything and successfully returns a
   * constant.
   */
  def success[a, b](token: b): PC[a, b] = combinator {
    in => (Some((token, in, 0)), None)
  }

  /*
   * A combinator that always fails.
   */
  def fail[a, b]: PC[a, b] = combinator {
    _ => (None, None)
  }

  /*
   * A combinator that successfully matches on the end of the input.
   */
  def eof[a]: PC[a, None.type] = named("EOF")(combinator {
    case x +: xs => (None, Some((Set(), 0)))
    case empty => (Some((None, empty, 0)), None)
  })

  /*
   * A combinator that does not consume anything and is always successful. If
   * there is input left, it returns the first element.
   */
  def head[a]: PC[a, Option[a]] = combinator {
    case x +: xs => (Some((Some(x), x +: xs, 0)), None)
    case empty => (Some((None, empty, 0)), None)
  }

  /*
   * A combinator that can match on a single input element, determined by some
   * predicate on this element.
   */
  def one[a](pred: a => Boolean): PC[a, a] = combinator {
    case x +: xs if pred(x) => (Some((x, xs, 1)), None)
    case _ => (None, Some((Set(), 0)))
  }

  /*
   * A combinator that can match on a single input element, determined by some
   * predicate on this and the following element (i.e. a lookahead).
   */
  def two[a](pred: (a, a) => Boolean): PC[a, a] = combinator {
    case x +: y +: xs if pred(x, y) => (Some((x, y +: xs, 1)), None)
    case _ => (None, Some((Set(), 0)))
  }

  /*
   * A combinator that matches on input elements equal to the parameter.
   */
  def item[a](in: a): PC[a, a] = named(in.toString)(one(in == _))

  /*
   * A combinator that chains multiple item-parsers into a list.
   */
  def items[a](ins: List[a]): PC[a, List[a]] = named(ins.mkString)(
    ((ins.map(item) :\ success[a, List[a]](Nil))(_ ** _ >> fuse))
  )

  /*
   * A combinator matching on zero or more repetitions of the parameter parser,
   * returning a (possibly empty) list of matches.
   */
  def star[a, b](inner: => PC[a, b]): PC[a, List[b]] = {
    lazy val innr = inner
    combinator {
      in: Stream[a] => {
        def lazyStar(acc: ParserResult[a, List[b]]): ParserResult[a, List[b]] =
          innr(acc._1.get._2) match {
          case (None, None) => {
            val (Some((bs, ys, i)), _) = acc
            (Some((bs.reverse, ys, i)), None)
          }
          case (None, Some((nms, j))) => {
            val (Some((bs, ys, i)), _) = acc
            (Some((bs.reverse, ys, i)), Some((nms, i + j)))
          }
          case (Some((b, ys, j)), _) => {
            val (Some((bs, _, i)), _) = acc
            lazyStar((Some((b :: bs, ys, i + j)), None))
          }
        }

        lazyStar((Some((Nil, in, 0)), None))
      }
    }
  }

  /*
   * A combinator matching on one or more repetitions of the parameter parser,
   * returning a (never empty) list of matches.
   */
  def plus[a, b](p: => PC[a, b]): PC[a, List[b]] =
    p ** star(p) >> fuse

  /*
   * A combinator optionally matching the parameter parser, returning an Option
   * of the inner result.
   */
  def optional[a, b](p: => PC[a, b]): PC[a, Option[b]] =
    p >> Option.apply || success(None)

  /*
   * A combinator which encloses some parser with elements on either side,
   * returning only the inside.
   */
  def enclosed[a, b](l: a, r: a, p: => PC[a, b]): PC[a, b] =
    enclosedP(item(l), item(r), p)

  /*
   * A combinator which encloses some parser with parsers on either side,
   * returning only the inside.
   */
  def enclosedP[a, b, c, d](l: => PC[a, c], r: => PC[a, d], p: => PC[a, b]): PC[a, b] =
    l *> p *< r

  /*
   * A combinator which adds some logging functionality to a parser, outputting
   * information on the start and result of an attempt.
   */
  def log[a, b](pr: => PC[a, b]): PC[a, b] = combinator {
    lazy val p = pr
    in => {
      println("Trying " + p.name + " on first char " + in.headOption.getOrElse('+'))
      val (pos, neg) = p(in)
      println("Positive: %s - Negative: %s".format(pos, neg))
      (pos, neg)
    }
  }
}
