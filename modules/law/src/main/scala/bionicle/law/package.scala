// Copyright (c) 2018 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package bionicle

package object law {

  type IsEq[A] = cats.kernel.laws.IsEq[A]
  val IsEq: cats.kernel.laws.IsEq.type = cats.kernel.laws.IsEq

  implicit final class IsEqArrow[A](val lhs: A) extends AnyVal {
    def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
  }

}
