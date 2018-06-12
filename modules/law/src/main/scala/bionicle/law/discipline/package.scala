// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle.law

import cats.Eq
import org.scalacheck.Prop

package object discipline {

  implicit def gemLawsIsEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    cats.kernel.laws.discipline.catsLawsIsEqToProp[A](isEq)

}
