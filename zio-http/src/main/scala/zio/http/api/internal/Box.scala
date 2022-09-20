package zio.http.api.internal

sealed trait Box[A] extends Product with Serializable { self =>
  import Box._
  def zip[B](that: Box[B])(implicit zipper: Zipper[A, B]): Box[zipper.Out] =
    Zip[A, B, zipper.Out](self, that, zipper)

  def map[B](f: A => B): Box[B] =
    Map(self, f)
}

object Box {

  def succeed[A](value: A): Box[A] = Succeed(value)

  final case class Succeed[A](value: A) extends Box[A]

  final case class Zip[A, B, C](lhs: Box[A], rhs: Box[B], zipper: Zipper.WithOut[A, B, C]) extends Box[C]

  final case class Map[A, B](box: Box[A], f: A => B) extends Box[B]

  def makeConstructor[A](box: Box[A]): Array[Any] => A = {
    box match {
      case Succeed(value) => _ => value

      case zip @ Zip(_, _, _) =>
        makeZipper(zip)

      case Map(box, f) =>
        val constructor = makeConstructor(box)
        args => f(constructor(args))
    }
  }

  private def makeZipper[A](zip: Zip[_, _, A]): Array[Any] => A =
//    if (countNestedZips(box) <= 2) {
    if (zip.zipper.leftSize == 1) {
//    if (zip.zipper.isInstanceOf[ABZipper[_, _]]) {
      // If there aren't enough nested zips, this optimization is not worth it,
      // use the original implementation.
      zip match {
        case Zip(lhs, rhs, zipper) =>
          val leftConstructor  = makeConstructor(lhs)
          val rightConstructor = makeConstructor(rhs)
          results => {
            val leftValue  = leftConstructor(results)
            val rightValue = rightConstructor(results)
            zipper.combine(leftValue, rightValue)
          }
      }
    } else {
      // If there 3 or more nested zips, we can optimize the zipper
      val zipper = makeZipperImpl(zip, 0)
      val size   = zip.zipper.resultSize
      println(s"size: $size for $zip")

      results => {
        val array: Array[Any] = Array.ofDim(size)
        zipper(results)(array)
        arrayToTuple[A](array, size)
      }
    }

  private def makeZipperImpl(zip: Zip[_, _, _], start: Int): Array[Any] => Array[Any] => Unit =
    zip match {
      case Zip(lhs: Zip[_, _, _], rhs: Zip[_, _, _], zipper) =>
        val zipper1 = makeZipperImpl(lhs, start)
        val zipper2 = makeZipperImpl(rhs, start + zipper.leftSize)

        results =>
          builder => {
            zipper1(results)(builder)
            zipper2(results)(builder)
          }

      case Zip(lhs, rhs: Zip[_, _, _], zipper) =>
        val lhsConstructor = makeConstructor(lhs)
        val rhsZipper      = makeZipperImpl(rhs, start + zipper.leftSize)

        results =>
          builder => {
            val lhsResult = lhsConstructor(results)
            zipper.spreadLeft(lhsResult, builder, start)
            rhsZipper(results)(builder)
          }

      case Zip(lhs: Zip[_, _, _], rhs, zipper) =>
        val lhsZipper      = makeZipperImpl(lhs, start)
        val rhsConstructor = makeConstructor(rhs)

        results =>
          builder => {
            lhsZipper(results)(builder)
            val rhsResult = rhsConstructor(results)
            zipper.spreadRight(rhsResult, builder, start + zipper.leftSize)
          }

      case Zip(lhs, rhs, zipper) =>
        val lhsConstructor = makeConstructor(lhs)
        val rhsConstructor = makeConstructor(rhs)

        results =>
          builder => {
            val lhsResult = lhsConstructor(results)
            val rhsResult = rhsConstructor(results)
            println(s"Zipping $lhsResult and $rhsResult")
            println(s"Zipper: ${zipper.leftSize} and ${zipper.rightSize}")
            println(s"Builder size ${builder.length}")
            zipper.spreadLeft(lhsResult, builder, start)
            zipper.spreadRight(rhsResult, builder, start + zipper.leftSize)
          }
    }

  private def countNestedZips(box: Box[_]): Int =
    box match {
      case Zip(left, right, _) => 1 + countNestedZips(left) + countNestedZips(right)
      case Map(_, _)           => 0
      case Succeed(_)          => 0
    }

  private def arrayToTuple[A](array: Array[Any], size: Int): A = {
    println(s"Converting array ${array.toList} to tuple of size $size")
    size match {
      case 1 => array(0).asInstanceOf[A]
      case 2 => (array(0), array(1)).asInstanceOf[A]
      case 3 => (array(0), array(1), array(2)).asInstanceOf[A]
      case 4 => (array(0), array(1), array(2), array(3)).asInstanceOf[A]
      case 5 => (array(0), array(1), array(2), array(3), array(4)).asInstanceOf[A]
      case 6 => (array(0), array(1), array(2), array(3), array(4), array(5)).asInstanceOf[A]
      case 7 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6)).asInstanceOf[A]
      case 8 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7)).asInstanceOf[A]
    }
  }
}

object BoxExample extends App {
  // TODO:
  //  1. write benchmarks against API DSL
  //  2. write a whole bunch more tests for this so we know there are no edge cases
  val example: Box[(Int, Unit, Double, Boolean, String)] =
    Box.succeed((1, ())) zip Box.succeed(1.0) zip Box.succeed(true) zip Box.succeed("a")

  val example2: Box[(Double, Int, Int)] =
    Box.succeed(2.0) zip Box.succeed(2) zip Box.succeed(3)

  val example3: Box[((Int, Unit, Double, Boolean, String), (Double, Int, Int))] =
    example zip example2

  val constructor = Box.makeConstructor(example3)
  val result      = constructor(Array())
  println(result)
}

sealed trait Zipper[A, B] extends Product with Serializable {
  type Out

  def combine(a: A, b: B): Out

  def leftSize: Int
  def rightSize: Int
  def resultSize: Int = leftSize + rightSize

  def spreadLeft(value: A, array: Array[Any], start: Int): Unit
  def spreadRight(value: B, array: Array[Any], start: Int): Unit
}

object Zipper extends LowPriorityZipper1 {
  type WithOut[A, B, Out0] = Zipper[A, B] { type Out = Out0 }

  final case class Zipper2[A, B, C]() extends Zipper[(A, B), C] {
    type Out = (A, B, C)

    override def combine(a: (A, B), b: C): (A, B, C) = (a._1, a._2, b)

    override def leftSize: Int  = 2
    override def rightSize: Int = 1

    override def spreadLeft(value: (A, B), array: Array[Any], start: Int): Unit = {
      array(start) = value._1
      array(start + 1) = value._2
    }

    override def spreadRight(value: C, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }

  }

  implicit def zipper2[A, B, C]: Zipper.WithOut[(A, B), C, (A, B, C)] = Zipper2()

  final case class Zipper3[A, B, C, D]() extends Zipper[(A, B, C), D] {
    type Out = (A, B, C, D)

    override def combine(a: (A, B, C), b: D): (A, B, C, D) = (a._1, a._2, a._3, b)

    override def leftSize: Int  = 3
    override def rightSize: Int = 1

    override def spreadLeft(value: (A, B, C), array: Array[Any], start: Int): Unit = {
      array(start) = value._1
      array(start + 1) = value._2
      array(start + 2) = value._3
    }

    override def spreadRight(value: D, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }
  }

  implicit def zipper3[A, B, C, D]: Zipper.WithOut[(A, B, C), D, (A, B, C, D)] = Zipper3()

  final case class Zipper4[A, B, C, D, E]() extends Zipper[(A, B, C, D), E] {
    type Out = (A, B, C, D, E)

    override def combine(a: (A, B, C, D), b: E): (A, B, C, D, E) = (a._1, a._2, a._3, a._4, b)

    override def leftSize: Int  = 4
    override def rightSize: Int = 1

    override def spreadLeft(value: (A, B, C, D), array: Array[Any], start: Int): Unit = {
      array(start) = value._1
      array(start + 1) = value._2
      array(start + 2) = value._3
      array(start + 3) = value._4
    }

    override def spreadRight(value: E, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }
  }

  implicit def zipper4[A, B, C, D, E]: Zipper.WithOut[(A, B, C, D), E, (A, B, C, D, E)] = Zipper4()
}

trait LowPriorityZipper1 extends LowPriorityZipper0 {
  implicit def leftUnitZipper[A]: Zipper.WithOut[Unit, A, A]  = LowPriorityZipper1.LeftUnitZipper[A]()
  implicit def rightUnitZipper[A]: Zipper.WithOut[A, Unit, A] = LowPriorityZipper1.RightUnitZipper[A]()
}

object LowPriorityZipper1 {
  final case class LeftUnitZipper[A]() extends Zipper[Unit, A] {
    type Out = A

    override def combine(a: Unit, b: A): A = b

    override def leftSize: Int  = 0
    override def rightSize: Int = 1

    override def spreadLeft(value: Unit, array: Array[Any], start: Int): Unit = {
      ()
    }

    override def spreadRight(value: A, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }
  }

  final case class RightUnitZipper[A]() extends Zipper[A, Unit] {
    type Out = A

    override def combine(a: A, b: Unit): A = a

    override def leftSize: Int  = 1
    override def rightSize: Int = 0

    override def spreadLeft(value: A, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }

    override def spreadRight(value: Unit, array: Array[Any], start: Int): Unit = {
      ()
    }
  }
}

trait LowPriorityZipper0 {
  implicit def abZipper[A, B]: Zipper.WithOut[A, B, (A, B)] = LowPriorityZipper0.ABZipper[A, B]()
}

object LowPriorityZipper0 {
  final case class ABZipper[A, B]() extends Zipper[A, B] {
    type Out = (A, B)

    override def combine(a: A, b: B): (A, B) = (a, b)

    override def leftSize: Int  = 1
    override def rightSize: Int = 1

    override def spreadLeft(value: A, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }

    override def spreadRight(value: B, array: Array[Any], start: Int): Unit = {
      array(start) = value
    }
  }
}
