// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle.law
package discipline

import cats.Eq
import bionicle.SplitEpi
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SplitEpiTests[A, B] extends FormatTests[A, B] {
  val splitEpiLaws: SplitEpiLaws[A, B]

  def splitEpi(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new DefaultRuleSet("SplitEpi",
      Some(format),
      "normalize"                -> forAll((a: A) => splitEpiLaws.normalize(a)),
      "normalized get roundtrip" -> forAll((a: A) => splitEpiLaws.normalizedGetRoundTrip(a)),
      "reverseGet roundtrip"     -> forAll((b: B) => splitEpiLaws.reverseGetRoundTrip(b)),
      "coverage"                 -> exists((a: A) => splitEpiLaws.demonstratesNormalization(a))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitEpiWith(ga: Gen[A])(
    implicit ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    splitEpi(Arbitrary(ga), ea, ab, eb)

}

object SplitEpiTests extends Laws {

  def apply[A, B](fab: SplitEpi[A, B]): SplitEpiTests[A, B] =
    new SplitEpiTests[A, B] {
      val formatLaws = new FormatLaws(fab.asFormat)
      val splitEpiLaws = new SplitEpiLaws(fab)
    }

}
