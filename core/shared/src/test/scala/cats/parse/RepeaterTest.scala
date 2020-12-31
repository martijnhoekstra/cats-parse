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

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import cats.data.NonEmptyList
import scala.util.Try
import org.scalacheck.Arbitrary
import cats.Defer

class RepeaterSuite extends munit.ScalaCheckSuite {

  test("partial parse fails in rep0") {
    val partial = Parser.length(1) ~ Parser.fail
    // we can't return empty list here
    assert(partial.rep0.parse("foo").isLeft)

    val p2 = Parser.string("f").orElse((Parser.string("boo") ~ Parser.string("p")).void)
    assert(p2.rep.parse("fboop").isRight)
    assert(p2.rep(2).parse("fboop").isRight)
    assert(p2.rep(3).parse("fboop").isLeft)
    assert(p2.rep.parse("fboof").isLeft)
  }

  property("rep anyChar accepts any char") {
    forAll { (ch: Char) =>
      assertEquals(Parser.anyChar.rep.parseAll(ch.toString()), Right(NonEmptyList(ch, Nil)))
    }
  }

  property("rep anyChar accepts any non-empty string") {
    forAll { (str: String) =>
      {
        val r = Parser.anyChar.rep
        if (str.nonEmpty) {
          assertEquals(r.parseAll(str), Right(NonEmptyList(str.head, str.tail.toList)))
        }
      }
    }
  }

  property("rep0 accepts an empty string") {
    forAll(ParserGen.gen) { (g: GenT[Parser]) =>
      assertEquals(g.fa.rep0.string.parseAll(""), Right(""))
    }
  }

  property("rep0 anyChar accepts any string") {
    forAll { (str: String) =>
      assertEquals(Parser.anyChar.rep0.parseAll(str), Right(str.toList))
    }
  }

  property("rep0 void of anyChar accepts any string") {
    forAll { (str: String) =>
      assertEquals(Parser.anyChar.rep0.void.parseAll(str), Right(()))
    }
  }

  test("reproduction") {
    val input = "aa"
    assertEquals(Parser.anyChar.rep0.void.parseAll(input), Right(()))
  }

  property("rep reads at least min times") {
    forAll(ParserGen.gen, Gen.posNum[Int], arbitrary[String]) {
      (g: GenT[Parser], min: Int, input: String) =>
        val p = g.fa
        val repeated = p.rep.withMin(min)
        for ((_, l) <- repeated.parse(input)) {
          assert(l.size >= min, s"read ${l.size} times with min $min")
        }
    }
  }

  property("rep reads at most max times") {
    forAll(ParserGen.gen, Gen.posNum[Int], arbitrary[String]) {
      (g: GenT[Parser], max: Int, input: String) =>
        val p = g.fa
        val repeated = p.rep.withMax(max)
        for ((_, l) <- repeated.parse(input)) {
          assert(l.size <= max, s"read ${l.size} times with max $max")
        }
    }
  }

  property("rep0 can be reimplemented with oneOf0 and defer") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      def rep0[A](pa: Parser[A]): Parser0[List[A]] =
        Defer[Parser0].fix[List[A]] { tail =>
          (pa ~ tail)
            .map { case (h, t) => h :: t }
            .orElse(Parser.pure(Nil))
        }

      val lst1 = rep0(genP.fa)
      val lst2 = genP.fa.rep0

      assertEquals(lst1.parse(str), lst2.parse(str))
    }
  }

  property("rep0 is consistent with rep") {
    forAll(ParserGen.gen, Gen.choose(0, Int.MaxValue), Arbitrary.arbitrary[String]) {
      (genP, min0, str) =>
        val min = min0 & Int.MaxValue
        val repA = genP.fa.rep0(min)
        val repB = genP.fa
          .rep(min)
          .map(_.toList)
          .orElse(
            if (min == 0) Parser.pure(Nil)
            else Parser.fail
          )

        assertEquals(repA.parse(str), repB.parse(str))
    }
  }

  property("rep0Sep with unit sep is the same as rep0") {
    forAll(ParserGen.gen, Gen.choose(0, Int.MaxValue), Arbitrary.arbitrary[String]) {
      (genP, min0, str) =>
        val min = min0 & Int.MaxValue
        val p1a = Parser.rep0Sep(genP.fa, min = min, sep = Parser.unit)
        val p1b = genP.fa.rep0(min = min)

        assertEquals(p1a.parse(str), p1b.parse(str))

        val min1 = if (min < 1) 1 else min
        val p2a = Parser.repSep(genP.fa, min = min1, sep = Parser.unit)
        val p2b = genP.fa.rep(min = min1)

        assertEquals(p2a.parse(str), p2b.parse(str))
    }
  }

  property("repSep with sep = fail is the same as parsing 1") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      assertEquals(
        genP.fa.parse(str),
        Parser.repSep(genP.fa, 1, Parser.fail).parse(str).map { case (rest, nel) =>
          (rest, nel.head)
        }
      )
    }
  }
}

class RepeaterCombinatorsTest extends ParserTest {
  property("repAs == rep.as") {
    forAll(ParserGen.gen, arbitrary[String]) { (genP, str) =>
      val p = genP.fa
      assertEquals(p.repAs[Vector[_]].parse(str), p.rep.as[Vector[_]].parse(str))
    }
  }

  property("repAs0 == rep0.as0") {
    forAll(ParserGen.gen, arbitrary[String]) { (genP, str) =>
      val p = genP.fa
      assertEquals(p.repAs0[Vector[_]].parse(str), p.rep0.as0[Vector[_]].parse(str))
    }
  }

  property("repSep == rep.sep") {
    forAll(ParserGen.gen, ParserGen.gen, Gen.posNum[Int], arbitrary[String]) {
      (genP, genSep, min, str) =>
        val p = genP.fa
        val sep = genSep.fa
        assertEquals(
          Parser.repSep(p, min, sep).parse(str),
          p.rep.withSep(sep).withMin(min).parse(str)
        )
    }
  }

  property("rep0Sep == rep0.sep") {
    forAll(
      ParserGen.gen,
      ParserGen.gen,
      Gen.oneOf(Gen.posNum[Int], Gen.const(0)), //Gen.const(-1)) <- fails, rep0Sep allows -1 as 0, withMin0 doesn't
      arbitrary[String]
    ) { (genP, genSep, min, str) =>
      val p = genP.fa
      val sep = genSep.fa
      assertEquals(
        Parser.rep0Sep(p, min, sep).parse(str),
        p.rep0.withSep(sep).withMin0(min).parse(str)
      )
    }
  }

  property("rep(min) == rep.min(min)") {
    forAll(
      ParserGen.gen,
      Gen.oneOf(Gen.posNum[Int], Gen.const(0), Gen.const(-1)),
      arbitrary[String]
    ) { (genP, min, str) =>
      val p = genP.fa
      assertEquals(Try(p.rep(min).parse(str)).toOption, Try(p.rep.withMin(min).parse(str)).toOption)
    }
  }

  property("rep0(min) == rep0.min0(min)") {
    forAll(
      ParserGen.gen,
      Gen.oneOf(Gen.posNum[Int], Gen.const(0), Gen.const(-1)),
      arbitrary[String]
    ) { (genP, min, str) =>
      val p = genP.fa
      assertEquals(
        Try(p.rep0(min).parse(str)).toOption,
        Try(p.rep0.withMin0(min).parse(str)).toOption
      )
    }
  }

}
