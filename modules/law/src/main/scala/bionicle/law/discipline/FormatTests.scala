// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle.law
package discipline

import cats.Eq
import cats.instances.option._
import bionicle.Format
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait FormatTests[A, B] extends Laws {
  val formatLaws: FormatLaws[A, B]

  def format(
    implicit aa: Arbitrary[A], ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("format",
      "normalize"        -> forAll((a: A) => formatLaws.normalize(a)),
      "parse roundtrip"  -> forAll((a: A) => formatLaws.parseRoundTrip(a)),
      "format roundtrip" -> forAll((b: B) => formatLaws.formatRoundTrip(b)),
      "coverage"         -> exists((a: A) => formatLaws.demonstratesNormalization(a))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def formatWith(ga: Gen[A])(
    implicit ea: Eq[A],
             ab: Arbitrary[B], eb: Eq[B]
  ): RuleSet =
    format(Arbitrary(ga), ea, ab, eb)

}

object FormatTests extends Laws {

  def apply[A, B](fab: Format[A, B]): FormatTests[A, B] =
    new FormatTests[A, B] {
      val formatLaws = new FormatLaws(fab)
    }

}
