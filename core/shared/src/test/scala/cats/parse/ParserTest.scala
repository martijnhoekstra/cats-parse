/*
 * Copyright (c) 2020 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.parse

import cats.{Eq, FlatMap, Defer, MonoidK, Monad}
import cats.data.NonEmptyList
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

import cats.implicits._
import scala.util.Random

trait ParserTest extends munit.ScalaCheckSuite {

  def parseTest[A: Eq](p: Parser0[A], str: String, a: A) =
    p.parse(str) match {
      case Right((_, res)) =>
        assert(Eq[A].eqv(a, res), s"expected: $a got $res")
      case Left(errs) =>
        assert(false, errs.toString)
    }

  def parseFail[A: Eq](p: Parser0[A], str: String) =
    p.parse(str) match {
      case Right(res) =>
        assert(false, s"expected to not parse, but found: $res")
      case Left(_) =>
        assert(true)
    }
  
  val tests: Int = if (BitSetUtil.isScalaJs) 50 else 2000

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)
}

class BasicParserTest extends ParserTest {

  test("pure works") {
    parseTest(Parser.pure(42), "anything", 42)
  }

  val fooP = Parser.string("foo")
  val barP = Parser.string("bar")
  val fooCIP = Parser.ignoreCase("foo")
  val cCIP = Parser.ignoreCase("a")
  val cCIP1 = Parser.ignoreCaseChar('a')
  val abcCI = Parser.ignoreCaseCharIn('a', 'b', 'c')

  test("string tests") {
    parseTest(fooP, "foobar", ())
    parseFail(fooP, "FOO")
    parseTest(fooCIP, "FoO", ())
    parseTest(cCIP, "A", ())
    parseTest(cCIP, "a", ())
    parseTest(cCIP1, "A", ())
    parseTest(cCIP1, "a", ())
    parseFail(fooP, "bar")

    parseTest(abcCI, "a", 'a')
    parseTest(abcCI, "A", 'A')
    parseTest(abcCI, "b", 'b')
    parseTest(abcCI, "B", 'B')
    parseTest(abcCI, "c", 'c')
    parseTest(abcCI, "C", 'C')
    parseFail(abcCI, "D")

    parseTest(Parser.oneOf(fooP :: barP :: Nil), "bar", ())
    parseTest(Parser.oneOf(fooP :: barP :: Nil), "foo", ())
    parseTest(Parser.stringIn(List("foo", "bar", "foobar")), "foo", ())
    parseTest(Parser.stringIn(List("foo", "bar", "foobar")), "bar", ())
    parseTest(Parser.stringIn(List("foo", "bar", "foobar")), "foobar", ())
  }

  test("product tests") {
    parseTest(Parser.product01(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product10(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product0(fooP, barP), "foobar", ((), ()))
  }

  test("longest match stringIn") {
    val alternatives = "foo" :: "foobar" :: "foofoo" :: "foobat" :: Nil
    parseTest(Parser.stringIn(alternatives).string, "foo", "foo")
    parseTest(Parser.stringIn(alternatives).string, "foobat", "foobat")
    parseTest(Parser.stringIn(List("foo", "foobar", "foofoo", "foobat")).string, "foot", "foo")
    parseTest(Parser.stringIn(List("foo", "foobar", "foofoo", "foobat")).string, "foobal", "foo")
  }
}

class SingleParserTest extends ParserTest {

  property("Parser0 on success replaces parsed value") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.as("something").parse(str)
      res0 match {
        case Left(_) => ()
        case Right((_, v)) => assertEquals(v, "something")
      }
    }
  }

  property("Parser.start and end work") {
    forAll { (s: String) =>
      if (s.isEmpty) {
        intercept[IllegalArgumentException] {
          Parser.string(s)
        }
      } else {
        val pa = Parser.string(s)
        assertEquals((Parser.start ~ pa ~ Parser.end).void.parse(s), Right(("", ())))
        assert((pa ~ Parser.start).parse(s).isLeft)
        assert((Parser.end ~ pa).parse(s).isLeft)
        assertEquals(
          (Parser.index ~ pa ~ Parser.index).map { case ((s, _), e) => e - s }.parse(s),
          Right(("", s.length))
        )
      }

      true
    }
  }

  property("Parser.length0 succeeds when the string is long enough") {
    forAll { (s: String, len: Int) =>
      if (len < 1) {
        intercept[IllegalArgumentException] {
          Parser.length(len)
        }
        assertEquals(Parser.length0(len).parse(s), Right((s, "")))
      } else {
        val pa = Parser.length0(len)
        val pa1 = Parser.length(len)

        val res = pa.parse(s)
        val res1 = pa1.parse(s)

        assertEquals(res, res1)

        res match {
          case Right((rest, first)) =>
            if (s.length >= len) {
              assertEquals(s.take(len), first)
              assertEquals(s.drop(len), rest)
            } else fail(s"expected to not parse: $rest, $first")
          case Left(Parser.Error(0, NonEmptyList(Parser.Expectation.Length(off, l, a), Nil))) =>
            assertEquals(off, 0)
            assertEquals(l, len)
            assertEquals(a, s.length)
          case Left(other) =>
            fail(s"unexpected error: $other")
        }
      }

      true
    }
  }
}

class VoidParserTest extends ParserTest {

  property("voided only changes the result") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)
      val r3 = FlatMap[Parser0].void(genP.fa).parse(str)
      val r4 = genP.fa.as(()).parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
      assertEquals(r2, r3)
      assertEquals(r2, r4)
    }
  }

  property("voided only changes the result Parser") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)
      val r3 = FlatMap[Parser].void(genP.fa).parse(str)
      val r4 = genP.fa.as(()).parse(str)
      val r5 = ((genP.fa.void: Parser0[Unit]) <* Monad[Parser0].unit).parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
      assertEquals(r2, r3)
      assertEquals(r2, r4)
      assertEquals(r2, r5)
    }
  }
}

class ErrorTest extends ParserTest {

  property("expected in errors gives valid offsets") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      genP.fa.parse(str) match {
        case Left(err) =>
          err.offsets.forall { off =>
            (0 <= off) && (off <= str.length)
          }
        case Right(_) => true
      }
    }
  }

  property("Parser.end gives the right error") {
    forAll { (str: String) =>
      Parser.end.parse(str) match {
        case Right((rest, _)) =>
          assertEquals(str, "")
          assertEquals(rest, "")
        case Left(Parser.Error(0, NonEmptyList(Parser.Expectation.EndOfString(off, len), Nil))) =>
          assertEquals(off, 0)
          assertEquals(len, str.length)
        case other =>
          fail(s"unexpected failure: $other")
      }
    }
  }
}

class OneOfTest extends ParserTest {

  property("oneOf0 nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen0), Gen.listOf(ParserGen.gen0), Arbitrary.arbitrary[String]) {
      (genP1, genP2, str) =>
        val oneOf = Parser.oneOf0((genP1 ::: genP2).map(_.fa))
        val oneOf2 = Parser.oneOf0(genP1.map(_.fa)).orElse(Parser.oneOf0(genP2.map(_.fa)))

        assertEquals(oneOf.parse(str), oneOf2.parse(str))
    }
  }

  property("oneOf nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen), Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) {
      (genP1, genP2, str) =>
        val oneOf = Parser.oneOf((genP1 ::: genP2).map(_.fa))
        val oneOf2 = Parser
          .oneOf(genP1.map(_.fa))
          .orElse(
            Parser.oneOf(genP2.map(_.fa))
          )

        assertEquals(oneOf.parse(str), oneOf2.parse(str))
    }
  }

  def orElse[A](p1: Parser0[A], p2: Parser0[A], str: String): Either[Parser.Error, (String, A)] = {
    if (p1 == Parser.Fail) p2.parse(str)
    else if (p2 == Parser.Fail) p1.parse(str)
    else
      p1.parse(str) match {
        case left @ Left(err) =>
          if (err.failedAtOffset == 0) {
            p2.parse(str)
              .leftMap { err1 =>
                if (err1.failedAtOffset == 0) {
                  val errs = err.expected ::: err1.expected
                  Parser.Error(err1.failedAtOffset, Parser.Expectation.unify(errs))
                } else err1
              }
          } else left
        case right => right
      }
  }

  property("oneOf0 composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      assertEquals(genP1.fa.orElse(genP2.fa).parse(str), orElse(genP1.fa, genP2.fa, str))
    }
  }

  property("oneOf composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      assertEquals(genP1.fa.orElse(genP2.fa).parse(str), orElse(genP1.fa, genP2.fa, str))
    }
  }

  property("oneOf0 same as foldLeft(fail)(_.orElse(_))") {
    forAll(Gen.listOf(ParserGen.gen0), Arbitrary.arbitrary[String]) { (genP1, str) =>
      val oneOfImpl = genP1.foldLeft(Parser.fail: Parser0[Any]) { (leftp, p) =>
        leftp.orElse(p.fa)
      }

      assertEquals(oneOfImpl.parse(str), Parser.oneOf0(genP1.map(_.fa)).parse(str))
    }
  }

  property("oneOf same as foldLeft(fail)(_.orElse(_))") {
    forAll(Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) { (genP1, str) =>
      val oneOfImpl = genP1.foldLeft(Parser.fail[Any]) { (leftp, p) => leftp.orElse(p.fa) }

      assertEquals(oneOfImpl.parse(str), Parser.oneOf(genP1.map(_.fa)).parse(str))
    }
  }
}

class IndexTest extends ParserTest {

  property("string can be recovered with index") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.string.parse(str)
      val r2 = (genP.fa ~ Parser.index).map { case (_, end) => str.substring(0, end) }.parse(str)

      assertEquals(r1.toOption, r2.toOption)
    }
  }
}

class BacktrackTest extends ParserTest {

  property("backtrack orElse pure always succeeds") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.orElse(Parser.pure(())): Parser0[Any]

      assert(p1.parse(str).isRight)
    }
  }

  property("backtrack.? pure always succeeds") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.?

      assert(p1.parse(str).isRight)
    }
  }

  property("a.backtrack either succeeds or fails at 0") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      a.fa.backtrack.parse(str) match {
        case Right(_) => ()
        case Left(err) => assertEquals(err.failedAtOffset, 0)
      }
    }
  }
}

class ProductTest extends ParserTest {

  property("a ~ b composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa ~ p2.fa
      val cres = composed.parse(str)

      val composed1 = Monad[Parser0].product(p1.fa, p2.fa)
      composed1.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length0(off) ~ p2.fa).map(_._2)
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a ~ b composes as expected parser1") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa ~ p2.fa
      val cres = composed.parse(str)

      val composed1 = FlatMap[Parser0].product(p1.fa, p2.fa)
      val cres1 = composed1.parse(str)
      assertEquals(cres, cres1)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = Parser.length0(off) *> p2.fa
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.with1 ~ b composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.with1 ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = Parser.length0(off) *> p2.fa
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.soft ~ b composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length0(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser.Error(fidx, errs) if (fidx == off) => Parser.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a1.soft ~ b composes as expected Parser") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length0(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser.Error(fidx, errs) if (fidx == off) => Parser.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.with1.soft ~ b1 composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.with1.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser.length0(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser.Error(fidx, errs) if (fidx == off) => Parser.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }
}

class MessageTest extends munit.ScalaCheckSuite {

  test("range messages seem to work") {
    val pa = Parser.charIn('0' to '9')
    assertEquals(pa.parse("z").toString, "Left(Error(0,NonEmptyList(InRange(0,0,9))))")
  }

}
class DeferTest extends ParserTest {

  test("defer does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser0].defer {
      cnt += 1
      Parser.string("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  test("defer does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser].defer {
      cnt += 1
      Parser.string("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }
}

class CharSuite extends ParserTest {

  property("charIn matches charWhere") {
    forAll { (cs: List[Char], str: String) =>
      val cset = cs.toSet
      val p1 = Parser.charIn(cs)
      val p2 = Parser.charWhere(cset)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("charIn matches charIn varargs") {
    forAll { (c0: Char, cs0: List[Char], str: String) =>
      val cs = c0 :: cs0
      val p1 = Parser.charIn(cs)
      val p2 = Parser.charIn(c0, cs0: _*)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }
}


class EquivalenceSuite extends ParserTest {

  property("charsWhile/charsWhere consistency") {
    forAll(
      Gen.choose(0, 100).flatMap(Gen.listOfN(_, Gen.choose(Char.MinValue, Char.MaxValue))),
      Arbitrary.arbitrary[String]
    ) { (chars, str) =>
      val pred = chars.toSet
      val p1a = Parser.charsWhile0(pred)
      val p1b = Parser.charWhere(pred).rep0.string
      assertEquals(p1a.parse(str), p1b.parse(str))

      val p2a = Parser.charsWhile(pred)
      val p2b = Parser.charWhere(pred).rep.string
      assertEquals(p2a.parse(str), p2b.parse(str))
    }
  }
}

class CatsInstancesSuite extends ParserTest {

  property("MonoidK[Parser0].empty never succeeds") {
    forAll { (str: String) =>
      assert(MonoidK[Parser0].empty.parse(str).isLeft)
      assert(MonoidK[Parser].empty.parse(str).isLeft)
    }
  }

  property("Monad.pure is an identity function") {
    forAll { (i: Int, str: String) =>
      assertEquals(Monad[Parser0].pure(i).parse(str), Right((str, i)))
    }
  }
}

class OrElseSuite extends ParserTest {

  property("p orElse p == p") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      val res1 = genP.fa.orElse(genP.fa).parse(str)
      assertEquals(res1, res0)
    }
  }

  property("p0 orElse p0 == p0") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      val res1 = genP.fa.orElse(genP.fa).parse(str)
      assertEquals(res1, res0)
    }
  }

  property("p1.backtrack.orElse(p2) succeeds if either p1 or p2 do (Parser0)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val ores = p1.fa.backtrack.orElse(p2.fa).parse(str)
      val r1 = p1.fa.parse(str)
      val r = if (r1.isLeft) p2.fa.parse(str) else r1
      (ores, r) match {
        case (Left(_), l) => assert(l.isLeft)
        case (ra, rb) => assertEquals(ra, rb)
      }
    }
  }

    property("p1.backtrack.orElse(p2) succeeds if either p1 or p2 do") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val ores = p1.fa.backtrack.orElse(p2.fa).parse(str)
      val r1 = p1.fa.parse(str)
      val r = if (r1.isLeft) p2.fa.parse(str) else r1
      (ores, r) match {
        case (Left(_), l) => assert(l.isLeft)
        case (ra, rb) => assertEquals(ra, rb)
      }
    }
  }
}

class ConsumeSuite extends ParserTest {

  property("Parser fails or consumes 1 or more") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      res0 match {
        case Left(_) => assert(true)
        case Right((s, _)) => assert(str != s)
      }
    }
  }
}

class CharsSuite extends ParserTest {

  test("charWhere(_ => true) == anyChar") {
    assertEquals(Parser.charWhere(_ => true), Parser.anyChar)
  }

  property("with1 *> and with1 <* work as expected") {
    forAll(ParserGen.gen0, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val rp1 = p1.fa.with1 *> p2.fa
      val rp2 = (p1.fa.with1 ~ p2.fa).map(_._2)
      assertEquals(rp1.parse(str), rp2.parse(str))

      val rp3 = p1.fa.with1 <* p2.fa
      val rp4 = (p1.fa.with1 ~ p2.fa).map(_._1)
      assertEquals(rp3.parse(str), rp4.parse(str))
    }
  }

  property("a1 *> b and a1 <* b") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      assertEquals(
        (p1.fa *> p2.fa).parse(str),
        Parser.product10(p1.fa.void, p2.fa).map(_._2).parse(str)
      )
      assertEquals(
        (p1.fa <* p2.fa).parse(str),
        Parser.product10(p1.fa, p2.fa.void).map(_._1).parse(str)
      )
    }
  }

  property("parse between open and close") {
    forAll(ParserGen.gen0, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (genP1, genP, genQ, str) =>
        val pa = genP1.fa.between(genP.fa, genQ.fa)
        val pb = genP.fa *> genP1.fa <* genQ.fa

        assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("surroundedBy consistent with between") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP, str) =>
      val pa = genP1.fa.between(genP.fa, genP.fa)
      val pb = genP1.fa.surroundedBy(genP.fa)

      assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("parse between open and close with Parser this") {
    forAll(ParserGen.gen, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (genP1, genP, genQ, str) =>
        val pa = genP1.fa.between(genP.fa, genQ.fa)
        val pb = genP.fa *> genP1.fa <* genQ.fa

        assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("surroundedBy consistent with between with Parser this") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP, str) =>
      val pa = genP1.fa.between(genP.fa, genP.fa)
      val pb = genP1.fa.surroundedBy(genP.fa)

      assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("parse between open and close with Parser args") {
    forAll(ParserGen.gen0, ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) {
      (genP1, genP, genQ, str) =>
        val pa = genP1.fa.with1.between(genP.fa, genQ.fa)
        val pb = genP.fa *> genP1.fa <* genQ.fa

        assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("surroundedBy consistent with between with Parser this") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP, str) =>
      val pa = genP1.fa.between(genP.fa, genP.fa)
      val pb = genP1.fa.surroundedBy(genP.fa)

      assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("exactly one of x or !x parse") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, str) =>
      val notx = !p1.fa

      val xor = p1.fa.parse(str).isRight ^ notx.parse(str).isRight
      assert(xor)
    }
  }

  property("if x ~ y matches then x ~ y.peek match") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (x, y, str) =>
      val m1 = (x.fa ~ y.fa).parse(str)
      val m2 = ((x.fa ~ y.fa.peek).map(_._1)).parse(str)

      assertEquals(m1.isRight, m2.isRight)
      if (m1.isRight) {
        assert(x.fa.parse(str) == m2)
      }
    }
  }

  property("if x matches then x.peek matches but returns the whole string and unit") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (x, str) =>
      if (x.fa.parse(str).isRight) {
        assertEquals(x.fa.peek.parse(str), Right((str, ())))
      }
    }
  }

  property("(a.soft ~ b) == a ~ b in success of expected (not partials)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left = a.fa.soft ~ b.fa
      val right = a.fa ~ b.fa
      val leftRes = left.parse(str).leftMap(_.expected)
      val rightRes = right.parse(str).leftMap(_.expected)
      assertEquals(leftRes, rightRes)
    }
  }

  property("(a.soft ~ b) == softProduct(a, b)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left = a.fa.soft ~ b.fa
      val right = Parser.softProduct0(a.fa, b.fa)
      assertEquals(left.parse(str), right.parse(str))
      assertEquals(
        (a.fa.soft *> b.fa).parse(str),
        Parser.softProduct0(a.fa.void, b.fa).map(_._2).parse(str)
      )
      assertEquals(
        (a.fa.soft <* b.fa).parse(str),
        Parser.softProduct0(a.fa, b.fa.void).map(_._1).parse(str)
      )
    }
  }

  property("(a1.soft ~ b) == softProduct(a, b)") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left1 = a.fa.soft ~ b.fa
      val right1 = Parser.softProduct10(a.fa, b.fa)
      assertEquals(left1.parse(str), right1.parse(str))

      val left2 = b.fa.soft ~ a.fa
      val right2 = Parser.softProduct01(b.fa, a.fa)
      assertEquals(left2.parse(str), right2.parse(str))

      assertEquals(
        (a.fa.soft *> b.fa).parse(str),
        Parser.softProduct10(a.fa.void, b.fa).map(_._2).parse(str)
      )
      assertEquals(
        (b.fa.with1.soft <* a.fa).parse(str),
        Parser.softProduct01(b.fa, a.fa.void).map(_._1).parse(str)
      )
      assertEquals(
        (b.fa.with1.soft *> a.fa).parse(str),
        Parser.softProduct01(b.fa.void, a.fa).map(_._2).parse(str)
      )
    }
  }

  property("Parser.until is like a search") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val p = Parser.until0(a.fa) *> a.fa
      def loopMatch(cnt: Int): Option[(String, a.A)] =
        (Parser.length0(cnt) *> a.fa).parse(str) match {
          case Right(res) => Some(res)
          case Left(_) if cnt > str.length => None
          case _ => loopMatch(cnt + 1)
        }

      assertEquals(p.parse(str).toOption, loopMatch(0))
    }
  }

  property("parseAll law") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pall = (a.fa <* Parser.end).parse(str).map(_._2)

      assertEquals(a.fa.parseAll(str), pall)
    }
  }

  property("BitSetUtil union works") {
    forAll { (cs: List[List[Char]]) =>
      val arys = cs.filter(_.nonEmpty).map(_.toArray.sorted)
      val bs = arys.map { ary => (ary(0).toInt, BitSetUtil.bitSetFor(ary)) }
      val sortedFlat = BitSetUtil.union(bs)
      assertEquals(sortedFlat.toSet, cs.flatten.toSet)
    }
  }

  /*
   * it would be nice if parsers were purely distributive, but they are not.
   * While cats Alternative laws do require some weak distributivity, Haskell
   * does not:
   *
   * https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus#Other_suggested_laws
   *
   * see a related cats discussion here:
   * https://github.com/typelevel/cats/pull/1345
   *
   * Instead, we have some weakened versions of distributive laws
   */
  property("b.orElse(c) ~ a == (b ~ a).orElse((!b) *> (c ~ a))") {
    forAll(ParserGen.gen0, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pb.orElse(pc) ~ pa
        val right = (pb ~ pa).orElse((!pb) *> (pc ~ pa))

        val leftRes = left.parse(str).toOption
        val rightRes = right.parse(str).toOption
        assertEquals(leftRes, rightRes)
    }
  }

  property("b.orElse(c) ~ a == (b ~ a).orElse((!b) *> (c ~ a))") {
    forAll(ParserGen.gen0, ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pb.orElse(pc) ~ pa
        val right = (pb ~ pa).orElse((!pb).with1 *> (pc ~ pa))

        val leftRes = left.parseAll(str).toOption
        val rightRes = right.parseAll(str).toOption
        if (leftRes.isDefined && rightRes.isDefined) {
          assertEquals(leftRes, rightRes)
        } else ()
    }
  }

  property("a ~ b.orElse(c) == (a.soft ~ b).orElse(a ~ c)") {
    forAll(ParserGen.gen0, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pa ~ pb.orElse(pc)
        val right = (pa.soft ~ pb).orElse(pa ~ pc)

        val leftRes = left.parse(str).toOption
        val rightRes = right.parse(str).toOption
        assertEquals(leftRes, rightRes)
    }
  }

  property("a ~ b.orElse(c) == (a.soft ~ b).orElse(a ~ c)") {
    forAll(ParserGen.gen0, ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pa ~ pb.orElse(pc)
        val right = (pa.soft.with1 ~ pb).orElse(pa.with1 ~ pc)

        val leftRes = left.parse(str).toOption
        val rightRes = right.parse(str).toOption
        assertEquals(leftRes, rightRes)
    }
  }

  property("a.backtrack.orElse(b) parses iff b.backtrack.orElse(a) (Parser0)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val pa = a.fa
      val pb = b.fa

      val left = pa.backtrack.orElse(pb)
      val right = pb.backtrack.orElse(pa)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes.toOption.isDefined, rightRes.toOption.isDefined)
    }
  }

  property("a.backtrack.orElse(b) parses iff b.backtrack.orElse(a)") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val pa = a.fa
      val pb = b.fa

      val left = pa.backtrack.orElse(pb)
      val right = pb.backtrack.orElse(pa)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes.toOption.isDefined, rightRes.toOption.isDefined)
    }
  }

  property("failWith returns the given error message") {
    forAll { (str: String, mes: String) =>
      assertEquals(
        Parser.failWith(mes).parse(str),
        Left(Parser.Error(0, NonEmptyList.of(Parser.Expectation.FailWith(0, mes))))
      )
    }
  }

  property("failWith.? returns None") {
    forAll { (str: String, mes: String) =>
      assertEquals(Parser.failWith(mes).?.parse(str), Right((str, None)))
    }
  }

  property("a.repAs0[Vector[A]] matches a.rep0.map(_.toVector)") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[a.A] = a.fa

      val left = pa.repAs0[Vector[a.A]]
      val right = pa.rep0.map(_.toVector)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.repAs[Vector[A]] matches a.rep.map(_.toList.toVector)") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[a.A] = a.fa

      val left = pa.repAs0[Vector[a.A]]
      val right = pa.rep0.map(_.toList.toVector)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.string.repAs0[String] matches a.string.rep0.map(_.mkString)") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[String] = a.fa.string

      val left = pa.repAs0[String]
      val right = pa.rep0.map(_.mkString)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.repAs0[Unit] matches a.rep0.void") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[a.A] = a.fa

      val left = pa.repAs0[Unit](Accumulator0.unitAccumulator0)
      val right = pa.rep0.void

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.peek == a.peek.peek") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa

      val left = pa.peek
      val right = pa.peek.peek

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.backtrack.peek.orElse(b.peek) == (a.backtrack.orElse(b)).peek") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val pa = a.fa.backtrack
      val pb = b.fa

      val left = pa.peek.orElse(pb.peek)
      val right = pa.orElse(pb).peek

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.peek == a.peek *> a.peek") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa.peek

      val left = pa
      val right = pa *> pa

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("!a == (!a) *> (!a)") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = !a.fa

      val left = pa
      val right = pa *> pa

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("!(!a) == a.peek") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa

      val left = (!(!pa))
      val right = pa.peek

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("!(!(!a)) == !a") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa

      val left = !(!(!pa))
      val right = !pa

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("!anyChar == end") {
    forAll { (str: String) =>
      val left = !Parser.anyChar
      val right = Parser.end

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("!fail == unit") {
    forAll { (str: String) =>
      val left = !Parser.fail
      val right = Parser.unit

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("!pure(_) == fail") {
    forAll { (str: String, i: Int) =>
      val left = !Parser.pure(i)
      val right = Parser.fail

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("anyChar.repAs0[String] parses the whole string") {
    forAll { (str: String) =>
      assertEquals(Parser.anyChar.repAs0[String].parse(str), Right(("", str)))
    }
  }

  property("string.soft ~ string is the same as concatenating the string") {
    forAll { (str1: String, str2: String, content: String) =>
      val left = (Parser.string0(str1).soft ~ Parser.string0(str2)).void
      val right = Parser.string0(str1 + str2)

      val leftRes = left.parse(content).toOption
      val rightRes = right.parse(content).toOption
      assertEquals(leftRes, rightRes)

    }
  }

  property("Parser.string(f).string == Parser.string(f).as(f)") {
    forAll { (f: String) =>
      if (f.length > 1)
        assertEquals(Parser.string(f).string, Parser.string(f).as(f))

    }
  }

  property("char(c).as(c) == charIn(c)") {
    forAll { (c: Char) =>
      assertEquals(Parser.char(c).as(c.toString), Parser.char(c).string)
      assertEquals(Parser.char(c).as(c), Parser.charIn(c))
      assertEquals(Parser.char(c).void.as(c), Parser.charIn(c))
      assertEquals(Parser.char(c).string.as(c), Parser.charIn(c))
    }
  }
}

class CombinatorEquivelenceSuite extends ParserTest {

  import ParserGen.{arbParser, arbParser0}

  property("select(pa.map(Left(_)))(pf) == (pa, pf).mapN((a, fn) => fn(a))") {
    forAll { (pa: Parser0[Int], pf: Parser0[Int => String], str: String) =>
      assertEquals(
        Parser.select0(pa.map(Left(_)))(pf).parse(str),
        (pa, pf).mapN((a, f) => f(a)).parse(str)
      )
    }
  }

  property("select1(pa.map(Left(_)))(pf) == (pa, pf).mapN((a, fn) => fn(a))") {
    forAll { (pa: Parser[Int], pf: Parser0[Int => String], str: String) =>
      assertEquals(
        Parser.select(pa.map(Left(_)))(pf).parse(str),
        (pa, pf).mapN((a, f) => f(a)).parse(str)
      )
    }
  }

  property("select(pa.map(Right(_)))(pf) == pa") {
    forAll { (pa: Parser0[String], pf: Parser0[Int => String], str: String) =>
      assertEquals(Parser.select0(pa.map(Right(_)))(pf).parse(str), pa.parse(str))
    }
  }

  property("select1(pa.map(Right(_)))(pf) == pa") {
    forAll { (pa: Parser[String], pf: Parser0[Int => String], str: String) =>
      assertEquals(Parser.select(pa.map(Right(_)))(pf).parse(str), pa.parse(str))
    }
  }

  property("p.filter(_ => true) == p") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.filter(_ => true).parse(str)
      val res1 = genP.fa.parse(str)
      assertEquals(res0, res1)
    }
  }

  property("p.filter(_ => false) fails") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res = genP.fa.filter(_ => false).parse(str)
      assert(res.isLeft)
    }
  }

  property("select on pure values works as expected") {
    forAll { (left: Option[Either[Int, String]], right: Option[Int => String], str: String) =>
      val pleft = left match {
        case Some(e) => Parser.pure(e)
        case None => Parser.fail
      }

      val pright = right match {
        case Some(f) => Parser.pure(f)
        case None => Parser.fail
      }

      assertEquals(
        Parser.select0(pleft)(pright).parse(str).toOption.map(_._2),
        left.flatMap {
          case Left(i) => right.map(_(i))
          case Right(s) =>
            // here even if right is None we have a result
            Some(s)
        }
      )
    }
  }

  property("mapFilter is the same as filter + map") {
    forAll { (pa: Parser0[Int], fn: Int => Option[String], str: String) =>
      val left = pa.mapFilter(fn)
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("mapFilter is the same as filter + map Parser") {
    forAll { (pa: Parser[Int], fn: Int => Option[String], str: String) =>
      val left = pa.mapFilter(fn)
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("collect is the same as filter + map") {
    forAll { (pa: Parser0[Int], fn: Int => Option[String], str: String) =>
      val left = pa.collect {
        case i if fn(i).isDefined => fn(i).get
      }
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("collect is the same as filter + map Parser") {
    forAll { (pa: Parser[Int], fn: Int => Option[String], str: String) =>
      val left = pa.collect {
        case i if fn(i).isDefined => fn(i).get
      }
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("eitherOr Parser0 works as expected") {
    forAll { (pa: Parser0[Int], pb: Parser0[String], str: String) =>
      val left = pa.eitherOr(pb).map {
        case Left(value) => value
        case Right(value) => value.toString()
      }
      val right = Parser.oneOf0(pa.map(_.toString) :: pb :: Nil)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("eitherOr Parser works as expected") {
    forAll { (pa: Parser[Int], pb: Parser[String], str: String) =>
      val left = pa.eitherOr(pb).map {
        case Left(value) => value
        case Right(value) => value.toString()
      }
      val right = Parser.oneOf(pa.map(_.toString) :: pb :: Nil)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("p.as(a).map(fn) == p.as(fn(a))") {
    forAll(ParserGen.gen, Gen.choose(0, 128), Gen.function1[Int, Int](Gen.choose(0, 128))) {
      (p, a, fn) =>
        assertEquals(p.fa.as(a).map(fn), p.fa.as(fn(a)))
    }

    forAll(ParserGen.gen0, Gen.choose(0, 128), Gen.function1[Int, Int](Gen.choose(0, 128))) {
      (p0, a, fn) =>
        assertEquals(p0.fa.as(a).map(fn), p0.fa.as(fn(a)))
    }
  }

  property("oneOf(string(s)*) success => stringIn(s*) success") {
    forAll { (ss0: List[String], toParse: String) =>
      val ss = ss0.filterNot(_.isEmpty)
      val oneOfs = Parser.oneOf(ss.map(Parser.string))
      val stringIn = Parser.stringIn(ss)
      if (oneOfs.parse(toParse).isRight) assert(stringIn.parse(toParse).isRight)
    }
  }

  property("stringIn(List(s)) == string(s)") {
    forAll { (s: String) =>
      if (s.nonEmpty)
        assertEquals(Parser.stringIn(List(s)), Parser.string(s))
    }
  }

  property("stringIn(List(s, s)) == string(s)") {
    forAll { (s: String) =>
      if (s.nonEmpty)
        assertEquals(Parser.stringIn(List(s, s)), Parser.string(s))
    }
  }

  property("string(s) matches  => stringIn(ss) matches if s in ss") {
    forAll { (s: String, ss0: List[String], toParse: String) =>
      val ss = ss0.filterNot(_.isEmpty)
      val ss1 = Random.shuffle(s :: ss)
      if (s.nonEmpty && Parser.string(s).parse(toParse).isRight)
        assert(Parser.stringIn(ss1).parse(toParse).isRight)
    }
  }

  property("Union parser is stringIn if alternatives have no common prefix") {
    forAll { (left0: List[String], right0: List[String], toParse: String) =>
      val left = left0.filterNot(_.isEmpty)
      val right = right0.filterNot(_.isEmpty)
      val noPrefix = left.forall { s => !right.exists(_.startsWith(s)) }
      if (noPrefix)
        assert(
          Parser.stringIn(left).orElse(Parser.stringIn(right)).parse(toParse).toOption ==
            Parser.stringIn(left ::: right).parse(toParse).toOption
        )
    }
  }

  property("stringIn parse longest match") {
    forAll { (ss0: List[String], toParse: String) =>
      val ss = ss0.filterNot(_.isEmpty)
      val left = Parser.stringIn(ss).parse(toParse).toOption
      val right = ss.filter(toParse.startsWith(_)).sortBy { s => -s.length }
      assertEquals(left.map(_._1), right.headOption)
    }
  }
}
