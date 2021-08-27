package icu.harx
package monoid

import org.scalacheck.{Gen, Prop}

import scala.language.postfixOps

trait Monoid[A]:
  def op(a1: A, a2: A): A
  def zero: A

extension [A](ma: Monoid[A])
  def dual: Monoid[A] = new:
    def op(a1: A, a2: A): A = this.op(a2, a1)
    lazy val zero: A        = this.zero

val stringMonoid: Monoid[String] = new:
  def op(s1: String, s2: String) = s1 + s2
  def zero                       = ""

def listMonoid[A]: Monoid[List[A]] = new:
  def op(la1: List[A], la2: List[A]) = la1 ::: la2
  def zero                           = Nil

object `练习 10.1`:
  val intAddition: Monoid[Int] = new:
    def op(n1: Int, n2: Int) = n1 + n2
    def zero                 = 0

  val intMultiplication: Monoid[Int] = new:
    def op(n1: Int, n2: Int) = n1 + n2
    def zero                 = 0

  val booleanOr: Monoid[Boolean] = new:
    def op(b1: Boolean, b2: Boolean) = b1 || b2
    def zero                         = false

  val booleanAnd: Monoid[Boolean] = new:
    def op(b1: Boolean, b2: Boolean) = b1 && b2
    def zero                         = true

object `练习 10.2`:
  def optionMonoid[A]: Monoid[Option[A]] = new:
    def op(oa1: Option[A], oa2: Option[A]) = oa1 orElse oa2
    def zero                               = None

object `练习 10.3`:
  type Endofunction[T] = T => T

  def endoMonoid[A]: Monoid[A => A] = new:
    def opReight(f1: Endofunction[A], f2: Endofunction[A]) = f1 compose f2
    def opLeft(f1: Endofunction[A], f2: Endofunction[A])   = f1 andThen f2
    def op(f1: Endofunction[A], f2: Endofunction[A])       = opLeft(f1, f2)
    def zero                                               = identity

object `练习 10.4`:
  def monoidLaws[A : Gen](monoidA: Monoid[A]): Prop =
    val genA = summon[Gen[A]]

    val tripleA =
      for
        x <- genA
        y <- genA
        z <- genA
      yield (x, y, z)

    val associativityProp =
      Prop.forAll(tripleA) { (x, y, z) =>
        monoidA.op(x, monoidA.op(y, z)) == monoidA.op(monoidA.op(x, y), z)
      }

    val identityProp =
      Prop.forAll(genA) { a =>
        monoidA.op(a, monoidA.zero) == a &&
        monoidA.op(monoidA.zero, a) == a
      }

    associativityProp && identityProp

def concatenate[A](la: List[A], ma: Monoid[A]): A =
  la.fold(ma.zero)(ma.op)

object `练习 10.5`:
  def foldMap[A, B](la: List[A], mb: Monoid[B])(f: A => B): B =
    (la map f).fold(mb.zero)(mb.op)

object `练习 10.6`:
  import `练习 10.3`.endoMonoid
  import `练习 10.5`.foldMap

  def foldRight[A, B](la: List[A])(z: B)(op: (A, B) => B): B =
    foldMap(la, endoMonoid[B])(op.curried)
      .apply(z)

  def foldLeft[A, B](la: List[A])(z: B)(op: (B, A) => B): B =
    val opDual = (a: A, b: B) => op(b, a)

    foldMap(la, endoMonoid[B].dual)(opDual.curried)
      .apply(z)

object `练习 10.7`:
  def foldMapV[A, B](va: IndexedSeq[A], mb: Monoid[B])(f: A => B): B =
    va.size match
      case 0 => mb.zero
      case 1 => f(va.head)
      case n =>
        val (l, r) = va.splitAt(n / 2)

        mb.op(foldMapV(l, mb)(f), foldMapV(r, mb)(f))

object `练习 10.8`:
  // todo Par
  type Par[A] = Nothing

  def par[A](m: Monoid[A]): Monoid[Par[A]]                                  = ???
  def parFoldMap[A, B](va: IndexedSeq[A], mb: Monoid[B])(f: A => B): Par[B] = ???
