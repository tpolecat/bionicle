// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle.law
package discipline

import cats.Eq
import bionicle.SplitMono
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SplitMonoTests[A, B] extends Laws {
  val laws: SplitMonoLaws[A, B]

  def splitMono(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("SplitMono",
      "normalize"                -> forAll((b: B) => laws.normalize(b)),
      "normalized get roundtrip" -> forAll((b: B) => laws.normalizedReverseGetRoundTrip(b)),
      "reverseGet roundtrip"     -> forAll((a: A) => laws.getRoundTrip(a)),
      "coverage"                 -> exists((b: B) => laws.demonstratesNormalization(b))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitMonoWith(ga: Gen[A])(
    implicit ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    splitMono(Arbitrary(ga), ea, ab, eb)

}

object SplitMonoTests extends Laws {

  def apply[A, B](fab: SplitMono[A, B]): SplitMonoTests[A, B] =
    new SplitMonoTests[A, B] {
      val laws = new SplitMonoLaws(fab)
    }

}
