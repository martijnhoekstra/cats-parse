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

import cats.{Id, FlatMap, Functor, Defer, MonoidK, Monad, Eval}
import cats.arrow.FunctionK
import org.scalacheck.{Arbitrary, Gen, Cogen}

sealed abstract class GenT[F[_]] { self =>
  type A
  val cogen: Cogen[A]
  val fa: F[A]

  def transform[G[_]](fk: FunctionK[F, G]): GenT[G] =
    new GenT[G] {
      type A = self.A
      val cogen = self.cogen
      val fa: G[A] = fk(self.fa)
    }

  def toId(implicit F: Functor[F]): F[GenT[Id]] =
    F.map(fa) { a =>
      new GenT[Id] {
        type A = self.A
        val cogen = self.cogen
        val fa = a
      }
    }

  override def toString: String = s"GenT($fa)"
}

object GenT {
  def apply[F[_], A0: Cogen](pa: F[A0]): GenT[F] =
    new GenT[F] {
      type A = A0
      val cogen = implicitly[Cogen[A0]]
      val fa = pa
    }
}


object ParserGen {
  implicit val functorGen: Functor[Gen] =
    new Functor[Gen] {
      def map[A, B](ga: Gen[A])(fn: A => B) = ga.map(fn)
    }

  def arbGen[A: Arbitrary: Cogen]: GenT[Gen] =
    GenT(Arbitrary.arbitrary[A])

  val pures: Gen[GenT[Gen]] =
    Gen.oneOf(arbGen[Int], arbGen[Boolean], arbGen[String], arbGen[(Int, Int)])

  val expect0: Gen[GenT[Parser0]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser.string0(str))
    }

  val ignoreCase0: Gen[GenT[Parser0]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser.ignoreCase0(str))
    }

  val charIn0: Gen[GenT[Parser0]] =
    Gen.oneOf(
      Arbitrary.arbitrary[List[Char]].map { cs =>
        GenT(Parser.charIn(cs): Parser0[Char])
      },
      Gen.const(GenT(Parser.anyChar: Parser0[Char]))
    )

  val charIn: Gen[GenT[Parser]] =
    Gen.oneOf(
      Arbitrary.arbitrary[List[Char]].map { cs =>
        GenT(Parser.charIn(cs))
      },
      Gen.const(GenT(Parser.anyChar))
    )

  val stringIn: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[List[String]].map { cs =>
      if (cs.exists(_.isEmpty)) GenT(Parser.fail: Parser[Unit])
      else GenT(Parser.stringIn(cs))
    }

  val expect1: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser.fail: Parser[Unit])
      else GenT(Parser.string(str))
    }

  val ignoreCase: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser.fail: Parser[Unit])
      else GenT(Parser.ignoreCase(str))
    }

  val fail: Gen[GenT[Parser0]] =
    Gen.const(GenT(Parser.fail: Parser0[Unit]))

  val failWith: Gen[GenT[Parser0]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser.failWith[Unit](str))
    }

  def void0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(Parser.void0(g.fa))

  def void(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.void(g.fa))

  def string0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(Parser.string0(g.fa))

  def string(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.string(g.fa))

  def backtrack0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(g.fa.backtrack)(g.cogen)

  def backtrack(g: GenT[Parser]): GenT[Parser] =
    GenT(g.fa.backtrack)(g.cogen)

  def defer0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(Defer[Parser0].defer(g.fa))(g.cogen)

  def defer(g: GenT[Parser]): GenT[Parser] =
    GenT(Defer[Parser].defer(g.fa))(g.cogen)

  def rep0(g: GenT[Parser]): GenT[Parser0] = {
    implicit val cg = g.cogen
    GenT[Parser0, List[g.A]](g.fa.rep0)
  }

  def rep(g: GenT[Parser]): GenT[Parser] = {
    implicit val cg = g.cogen
    GenT[Parser, List[g.A]](g.fa.rep.map(_.toList))
  }

  def product0(ga: GenT[Parser0], gb: GenT[Parser0]): Gen[GenT[Parser0]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].product(ga.fa, gb.fa)),
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].map2(ga.fa, gb.fa)((_, _))),
      GenT[Parser0, (ga.A, gb.A)](
        FlatMap[Parser0].map2Eval(ga.fa, Eval.later(gb.fa))((_, _)).value
      ),
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].map2Eval(ga.fa, Eval.now(gb.fa))((_, _)).value)
    )
  }

  def softProduct0(ga: GenT[Parser0], gb: GenT[Parser0]): Gen[GenT[Parser0]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.const(
      GenT[Parser0, (ga.A, gb.A)](ga.fa.soft ~ gb.fa)
    )
  }

  def product(ga: GenT[Parser], gb: GenT[Parser]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].product(ga.fa, gb.fa)),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2(ga.fa, gb.fa)((_, _))),
      GenT[Parser, (ga.A, gb.A)](
        FlatMap[Parser].map2Eval(ga.fa, Eval.later(gb.fa))((_, _)).value
      ),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2Eval(ga.fa, Eval.now(gb.fa))((_, _)).value)
    )
  }

  def product10(ga: GenT[Parser], gb: GenT[Parser0]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser, (ga.A, gb.A)](Parser.product10(ga.fa, gb.fa)),
      GenT[Parser, ga.A](ga.fa <* gb.fa),
      GenT[Parser, gb.A](ga.fa *> gb.fa),
      GenT[Parser, (ga.A, ga.A)](Parser.product10(ga.fa, ga.fa))
    )
  }

  def softProduct10(ga: GenT[Parser], gb: GenT[Parser0]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      // left is Parser
      GenT[Parser, (ga.A, gb.A)](ga.fa.soft ~ gb.fa),
      // right is Parser
      GenT[Parser, (gb.A, ga.A)](gb.fa.with1.soft ~ ga.fa),
      // both are parser1
      GenT[Parser, (ga.A, ga.A)](ga.fa.soft ~ ga.fa)
    )
  }

  def mapped(ga: GenT[Parser0]): Gen[GenT[Parser0]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser0].map(ga.fa)(fn))
        )
      }
    }
  }

  def mapped1(ga: GenT[Parser]): Gen[GenT[Parser]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser].map(ga.fa)(fn))
        )
      }
    }
  }

  abstract class FlatMap[F[_], B] {
    type A
    val init: F[A]
    val fn: A => F[B]
  }

  def selected(ga: Gen[GenT[Parser0]]): Gen[GenT[Parser0]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser0[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[Parser0[genRes1.A => genRes2.A]] =
        ga.flatMap { init =>
          val mapFn: Gen[init.A => (genRes1.A => genRes2.A)] =
            Gen.function1(Gen.function1(genRes2.fa)(genRes1.cogen))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }

      Gen.zip(genPR, gfn).map { case (pab, fn) =>
        GenT(Parser.select0(pab)(fn))(genRes2.cogen)
      }
    }

  def flatMapped(ga: Gen[GenT[Parser0]]): Gen[GenT[Parser0]] =
    Gen.zip(ga, pures).flatMap { case (parser, genRes) =>
      val genPR: Gen[Parser0[genRes.A]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => genRes.A] =
            Gen.function1(genRes.fa)(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[parser.A => Parser0[genRes.A]] =
        Gen.function1(genPR)(parser.cogen)

      gfn.flatMap { fn =>
        Gen.oneOf(
          GenT(parser.fa.flatMap(fn))(genRes.cogen),
          GenT(FlatMap[Parser0].flatMap(parser.fa)(fn))(genRes.cogen)
        )
      }
    }

  // if we use a Parser0 here, we could loop forever parsing nothing
  def tailRecM(ga: Gen[GenT[Parser]]): Gen[GenT[Parser0]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser0[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn = Gen.function1(genPR)(genRes1.cogen)

      Gen
        .zip(genRes1.fa, gfn)
        .map { case (init, fn) =>
          GenT(Monad[Parser0].tailRecM(init)(fn))(genRes2.cogen)
        }
    }

  def tailRecM1(ga: Gen[GenT[Parser]]): Gen[GenT[Parser]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn = Gen.function1(genPR)(genRes1.cogen)

      Gen
        .zip(genRes1.fa, gfn)
        .map { case (init, fn) =>
          GenT(FlatMap[Parser].tailRecM(init)(fn))(genRes2.cogen)
        }
    }

  def selected1(ga1: Gen[GenT[Parser]], ga0: Gen[GenT[Parser0]]): Gen[GenT[Parser]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR1: Gen[Parser[Either[genRes1.A, genRes2.A]]] = {
        ga1.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[Parser0[genRes1.A => genRes2.A]] =
        ga0.flatMap { init =>
          val mapFn: Gen[init.A => (genRes1.A => genRes2.A)] =
            Gen.function1(Gen.function1(genRes2.fa)(genRes1.cogen))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }

      Gen.zip(genPR1, gfn).map { case (pab, fn) =>
        GenT(Parser.select(pab)(fn))(genRes2.cogen)
      }
    }

  def flatMapped1(ga: Gen[GenT[Parser0]], ga1: Gen[GenT[Parser]]): Gen[GenT[Parser]] =
    Gen.zip(ga, ga1, pures).flatMap { case (parser, parser1, genRes) =>
      val genPR: Gen[Parser[genRes.A]] = {
        ga1.flatMap { init =>
          val mapFn: Gen[init.A => genRes.A] =
            Gen.function1(genRes.fa)(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[parser.A => Parser[genRes.A]] =
        Gen.function1(genPR)(parser.cogen)

      val gfn1: Gen[parser1.A => Parser[genRes.A]] =
        Gen.function1(genPR)(parser1.cogen)

      Gen.frequency(
        (
          2,
          gfn1.flatMap { fn =>
            Gen.oneOf(
              GenT(parser1.fa.flatMap(fn))(genRes.cogen), // 1 -> 0
              GenT(FlatMap[Parser].flatMap(parser1.fa)(fn))(genRes.cogen) // 1 -> 1
            )
          }
        ),
        (
          1,
          gfn.map { fn =>
            GenT(parser.fa.with1.flatMap(fn))(genRes.cogen) // 0 -> 1
          }
        )
      )
    }

  def orElse0(ga: GenT[Parser0], gb: GenT[Parser0], res: GenT[Gen]): Gen[GenT[Parser0]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).flatMap { case (f1, f2) =>
      Gen.oneOf(
        GenT(ga.fa.map(f1).orElse(gb.fa.map(f2))),
        GenT(MonoidK[Parser0].combineK(ga.fa.map(f1), gb.fa.map(f2)))
      )
    }
  }

  def orElse(ga: GenT[Parser], gb: GenT[Parser], res: GenT[Gen]): Gen[GenT[Parser]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).flatMap { case (f1, f2) =>
      Gen.oneOf(
        GenT(ga.fa.map(f1).orElse(gb.fa.map(f2))),
        GenT(MonoidK[Parser].combineK(ga.fa.map(f1), gb.fa.map(f2)))
      )
    }
  }

  // Generate a random parser
  lazy val gen0: Gen[GenT[Parser0]] = {
    val rec = Gen.lzy(gen0)

    Gen.frequency(
      (
        3,
        pures
          .flatMap(_.toId)
          .map(_.transform(new FunctionK[Id, Parser0] {
            def apply[A](g: Id[A]): Parser0[A] = Parser.pure(g)
          }))
      ),
      (5, expect0),
      (1, ignoreCase0),
      (5, charIn0),
      (1, Gen.oneOf(GenT(Parser.start), GenT(Parser.end), GenT(Parser.index))),
      (1, fail),
      (1, failWith),
      (1, rec.map(void0(_))),
      (1, rec.map(string0(_))),
      (1, rec.map(backtrack0(_))),
      (1, rec.map(defer0(_))),
      (1, rec.map { gen => GenT(!gen.fa) }),
      (1, Gen.lzy(gen.map(rep0(_)))),
      (1, rec.flatMap(mapped(_))),
      (1, rec.flatMap(selected(_))),
      (1, tailRecM(Gen.lzy(gen))),
      (1, Gen.choose(0, 10).map { l => GenT(Parser.length0(l)) }),
      (1, flatMapped(rec)),
      (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => product0(g1, g2) }),
      (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => softProduct0(g1, g2) }),
      (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse0(g1, g2, p) })
    )
  }

  // Generate a random parser
  lazy val gen: Gen[GenT[Parser]] = {
    val rec = Gen.lzy(gen)

    Gen.frequency(
      (8, expect1),
      (2, ignoreCase),
      (8, charIn),
      (8, stringIn),
      (1, Gen.choose(Char.MinValue, Char.MaxValue).map { c => GenT(Parser.char(c)) }),
      (2, rec.map(void(_))),
      (2, rec.map(string(_))),
      (2, rec.map(backtrack(_))),
      (1, rec.map(defer(_))),
      (1, rec.map(rep(_))),
      (1, selected1(rec, gen0)),
      (1, rec.flatMap(mapped1(_))),
      (1, flatMapped1(gen0, rec)),
      (1, tailRecM1(rec)),
      (1, Gen.choose(1, 10).map { l => GenT(Parser.length(l)) }),
      (
        2,
        Gen.frequency(
          (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => product(g1, g2) }),
          (1, Gen.zip(rec, gen0).flatMap { case (g1, g2) => product10(g1, g2) }),
          (1, Gen.zip(rec, gen0).flatMap { case (g1, g2) => softProduct10(g1, g2) }),
          (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse(g1, g2, p) })
        )
      )
    )
  }

  def genParser0[A](genA: Gen[A]): Gen[Parser0[A]] =
    for {
      genT <- gen0
      fn <- Gen.function1(genA)(genT.cogen)
    } yield genT.fa.map(fn)

  def genParser[A](genA: Gen[A]): Gen[Parser[A]] =
    for {
      genT <- gen
      fn <- Gen.function1(genA)(genT.cogen)
    } yield genT.fa.map(fn)

  implicit def arbParser0[A: Arbitrary]: Arbitrary[Parser0[A]] =
    Arbitrary(genParser0(Arbitrary.arbitrary[A]))

  implicit def arbParser[A: Arbitrary]: Arbitrary[Parser[A]] =
    Arbitrary(genParser(Arbitrary.arbitrary[A]))
}