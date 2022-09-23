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

  final case class Zip[A, B, C](lhs: Box[A], rhs: Box[B], zipper: Zipper.WithOut[A, B, C]) extends Box[C] {
    def shouldOptimizeRight: Boolean = rhs match {
      case Zip(_, _, rightZipper) => rightZipper.resultSize == zipper.rightSize
      case _                      => true
    }

    def shouldOptimizeLeft: Boolean = lhs match {
      case Zip(_, _, leftZipper) => leftZipper.resultSize == zipper.leftSize
      case _                     => true
    }
  }

  final case class Map[A, B](box: Box[A], f: A => B) extends Box[B]

  def makeConstructor[A](box: Box[A], optimizeZip: Boolean = true): Array[Any] => A = {
    box match {
      case Succeed(value) => _ => value

      case zip @ Zip(_, _, _) if optimizeZip =>
        makeZipper(zip)

      case Zip(lhs, rhs, zipper) =>
        val lhsCons = makeConstructor(lhs, optimizeZip)
        val rhsCons = makeConstructor(rhs, optimizeZip)
        results => zipper.combine(lhsCons(results), rhsCons(results))

      case Map(box, f) =>
        val constructor = makeConstructor(box, optimizeZip)
        args => f(constructor(args))
    }
  }

  private def makeZipper[A](zip: Zip[_, _, A]): Array[Any] => A = {
    if (!shouldOptimize(zip) || countNestedZips(zip) < 2) {
      zip match {
        case Zip(lhs, rhs, zipper) =>
          val lhsCons = makeConstructor(lhs)
          val rhsCons = makeConstructor(rhs)
          results => zipper.combine(lhsCons(results), rhsCons(results))
      }
    } else {
      // If there 3 or more nested zips, we can optimize the zipper
      val size   = zip.zipper.resultSize
      val zipper = makeZipperImpl(zip, 0, size - 1)
      results => {
        val array: Array[Any] = Array.ofDim(size)
        zipper(results)(array)
        arrayToTuple[A](array, size)
      }
    }
  }

  private def makeZipperImpl(zip: Zip[_, _, _], start: Int, end: Int): Array[Any] => Array[Any] => Unit =
    zip match {
      case Zip(lhs: Zip[_, _, _], rhs: Zip[_, _, _], zipper) if !shouldOptimize(zip) =>
        val lhsConstructor = makeZipper(lhs)
        val rhsConstructor = makeZipper(rhs)
        results =>
          builder => {
            val lhsResult = lhsConstructor(results)
            val rhsResult = rhsConstructor(results)
            zipper.spreadLeft(lhsResult, builder, start)
            zipper.spreadRight(rhsResult, builder, start + zipper.leftSize)
          }

      case Zip(lhs: Zip[_, _, _], rhs: Zip[_, _, _], zipper) =>
        val zipper1 = makeZipperImpl(lhs, start, zipper.leftSize)
        val zipper2 = makeZipperImpl(rhs, start + zipper.leftSize, end)

        results =>
          builder => {
            zipper1(results)(builder)
            zipper2(results)(builder)
          }

      case Zip(lhs, rhs: Zip[_, _, _], zipper) =>
        val lhsConstructor = makeConstructor(lhs)
        val rhsZipper      = makeZipperImpl(rhs, start + zipper.leftSize, end)

        results =>
          builder => {
            val lhsResult = lhsConstructor(results)
            zipper.spreadLeft(lhsResult, builder, start)
            rhsZipper(results)(builder)
          }

      case Zip(lhs: Zip[_, _, _], rhs, zipper) =>
        val lhsZipper      = makeZipperImpl(lhs, start, end)
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
//            println(s"Zipping $lhsResult and $rhsResult")
//            println(s"Zipper: ${zipper.leftSize} and ${zipper.rightSize}")
//            println(s"Builder size ${builder.length}")
            zipper.spreadLeft(lhsResult, builder, start)
            zipper.spreadRight(rhsResult, builder, start + zipper.leftSize)
          }
    }

  private def countNestedZips(box: Box[_]): Int =
    box match {
      case zip @ Zip(left, right, _) =>
        val leftZips  = if (zip.shouldOptimizeLeft) countNestedZips(left) else 0
        val rightZips = if (zip.shouldOptimizeRight) countNestedZips(right) else 0
        1 + leftZips + rightZips
      case Map(_, _)                 => 0
      case Succeed(_)                => 0
    }

  // only optimize if the resultSize of the nested zipper on either side is of the expected size
  def shouldOptimize(zip: Zip[_, _, _]): Boolean = {
    zip match {
      case Zip(left: Zip[_, _, _], right: Zip[_, _, _], zipper) =>
        left.zipper.resultSize == zipper.leftSize && right.zipper.resultSize == zipper.rightSize
      case Zip(left: Zip[_, _, _], _, zipper)                   =>
        left.zipper.resultSize == zipper.leftSize
      case Zip(_, right: Zip[_, _, _], zipper)                  =>
        right.zipper.resultSize == zipper.rightSize
      case _                                                    => true
    }
  }

  private def arrayToTuple[A](as: Array[Any], size: Int): A = {
//    println(s"Converting array ${as.toList} to tuple of size $size")
    size match {
      case 1  => as(0).asInstanceOf[A]
      case 2  => (as(0), as(1)).asInstanceOf[A]
      case 3  => (as(0), as(1), as(2)).asInstanceOf[A]
      case 4  => (as(0), as(1), as(2), as(3)).asInstanceOf[A]
      case 5  => (as(0), as(1), as(2), as(3), as(4)).asInstanceOf[A]
      case 6  => (as(0), as(1), as(2), as(3), as(4), as(5)).asInstanceOf[A]
      case 7  => (as(0), as(1), as(2), as(3), as(4), as(5), as(6)).asInstanceOf[A]
      case 8  => (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7)).asInstanceOf[A]
      case 9  => (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7), as(8)).asInstanceOf[A]
      case 10 => (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7), as(8), as(9)).asInstanceOf[A]
      case 11 => (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7), as(8), as(9), as(10)).asInstanceOf[A]
      case 12 => (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7), as(8), as(9), as(10), as(11)).asInstanceOf[A]
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

  val example3: Box[(Int, Unit, Double, Boolean, String, (Double, Int, Int))] =
    example zip example2

  val example4: Box[(Int, Int, (Double, Int, Int), Int, Int)] =
    Box.Succeed(()) zip Box.succeed((10, 20)) zip example2 zip Box.succeed(4) zip Box.succeed(()) zip Box.succeed(5)

  val constructor = Box.makeConstructor(example4, false)
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

  implicit def leftUnitZipper[A]: Zipper.WithOut[Unit, A, A]  = LowPriorityZipper1.LeftUnitZipper[A]()
  implicit def rightUnitZipper[A]: Zipper.WithOut[A, Unit, A] = LowPriorityZipper1.RightUnitZipper[A]()

}

trait LowPriorityZipper1 extends LowPriorityZipper0 {
  implicit def zipper2[A, B, C]: Zipper.WithOut[(A, B), C, (A, B, C)]                                     = Zipper2()
  implicit def zipper3[A, B, C, D]: Zipper.WithOut[(A, B, C), D, (A, B, C, D)]                            = Zipper3()
  implicit def zipper4[A, B, C, D, E]: Zipper.WithOut[(A, B, C, D), E, (A, B, C, D, E)]                   = Zipper4()
  implicit def zipper5[A, B, C, D, E, F]: Zipper.WithOut[(A, B, C, D, E), F, (A, B, C, D, E, F)]          = Zipper5()
  implicit def zipper6[A, B, C, D, E, F, G]: Zipper.WithOut[(A, B, C, D, E, F), G, (A, B, C, D, E, F, G)] = Zipper6()
}

final case class Zipper2[A, B, C]() extends Zipper[(A, B), C] {
  type Out = (A, B, C)

  override def combine(a: (A, B), b: C): (A, B, C) = (a._1, a._2, b)

  override def leftSize: Int = 2

  override def rightSize: Int = 1

  override def spreadLeft(value: (A, B), array: Array[Any], start: Int): Unit = {
    array(start) = value._1
    array(start + 1) = value._2
  }

  override def spreadRight(value: C, array: Array[Any], start: Int): Unit = {
    array(start) = value
  }

}

final case class Zipper3[A, B, C, D]() extends Zipper[(A, B, C), D] {
  type Out = (A, B, C, D)

  override def combine(a: (A, B, C), b: D): (A, B, C, D) = (a._1, a._2, a._3, b)

  override def leftSize: Int = 3

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

final case class Zipper4[A, B, C, D, E]() extends Zipper[(A, B, C, D), E] {
  type Out = (A, B, C, D, E)

  override def combine(a: (A, B, C, D), b: E): (A, B, C, D, E) = (a._1, a._2, a._3, a._4, b)

  override def leftSize: Int = 4

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

final case class Zipper5[A, B, C, D, E, F]() extends Zipper[(A, B, C, D, E), F] {
  type Out = (A, B, C, D, E, F)

  override def combine(a: (A, B, C, D, E), b: F): (A, B, C, D, E, F) = (a._1, a._2, a._3, a._4, a._5, b)

  override def leftSize: Int = 5

  override def rightSize: Int = 1

  override def spreadLeft(value: (A, B, C, D, E), array: Array[Any], start: Int): Unit = {
    array(start) = value._1
    array(start + 1) = value._2
    array(start + 2) = value._3
    array(start + 3) = value._4
    array(start + 4) = value._5
  }

  override def spreadRight(value: F, array: Array[Any], start: Int): Unit = {
    array(start) = value
  }
}

final case class Zipper6[A, B, C, D, E, F, G]() extends Zipper[(A, B, C, D, E, F), G] {
  type Out = (A, B, C, D, E, F, G)

  override def combine(a: (A, B, C, D, E, F), b: G): (A, B, C, D, E, F, G) = (a._1, a._2, a._3, a._4, a._5, a._6, b)

  override def leftSize: Int = 6

  override def rightSize: Int = 1

  override def spreadLeft(value: (A, B, C, D, E, F), array: Array[Any], start: Int): Unit = {
    array(start) = value._1
    array(start + 1) = value._2
    array(start + 2) = value._3
    array(start + 3) = value._4
    array(start + 4) = value._5
    array(start + 5) = value._6
  }

  override def spreadRight(value: G, array: Array[Any], start: Int): Unit = {
    array(start) = value
  }
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
