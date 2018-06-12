// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle.law

import cats.Eq
import cats.implicits._
import bionicle.SplitMono

final case class SplitMonoLaws[A, B](fab: SplitMono[A, B]) {

  def normalize(b: B): IsEq[A] =
    fab.reverseGet(fab.normalize(b)) <-> fab.reverseGet(b)

  def normalizedReverseGetRoundTrip(b: B): IsEq[B] = {
    val bʹ = fab.normalize(b)
    (fab.reverseGet andThen fab.get)(bʹ) <-> bʹ
  }

  def getRoundTrip(a: A): IsEq[A] =
    (fab.get andThen fab.reverseGet)(a) <-> a

  // True if `a` is parsable but not in normal form. The existence of such a value in our test data
  // will show that `normalize` and `parseRoundTrup` are actually testing something.
  def demonstratesNormalization(b: B)(implicit ev: Eq[B]): Boolean =
    (fab.reverseGet andThen fab.get)(b) =!= b

}
