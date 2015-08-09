package champs.parsers

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by dwight on 09/08/15.
 */
class GroupedRegexParser extends RegexParsers{
  /** A parser that matches a regex string and returns the Match */
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(matched,
            in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }
}
