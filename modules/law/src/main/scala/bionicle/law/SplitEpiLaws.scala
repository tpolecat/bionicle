// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle.law

import cats.Eq
import cats.implicits._
import bionicle.SplitEpi

final case class SplitEpiLaws[A, B](fab: SplitEpi[A, B]) {

  def normalize(a: A): IsEq[B] =
    fab.get(fab.normalize(a)) <-> fab.get(a)

  def normalizedGetRoundTrip(a: A): IsEq[A] = {
    val aʹ = fab.normalize(a)
    (fab.get andThen fab.reverseGet)(aʹ) <-> aʹ
  }

  def reverseGetRoundTrip(b: B): IsEq[B] =
    (fab.reverseGet andThen fab.get)(b) <-> b

  // True if `a` is parsable but not in normal form. The existence of such a value in our test data
  // will show that `normalize` and `parseRoundTrup` are actually testing something.
  def demonstratesNormalization(a: A)(implicit ev: Eq[A]): Boolean =
    (fab.get andThen fab.reverseGet)(a) =!= a

}
