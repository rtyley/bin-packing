package com.madgag.algo.packing.binpacking


import cats.syntax.all._
import cats.{Group, Order}
import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector
import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.BinSelector
import com.madgag.algo.packing.binpacking.BinPacking.Size._
import com.madgag.scala.collection.decorators._
import spire.algebra.{CoordinateSpace, Field}
import spire.implicits.coordinateSpaceOps
import com.madgag.algo.packing.binpacking.BinPacking.CanFit._

import scala.collection.immutable.SortedMap
import scala.collection.mutable

/**
 * We want a way to track arbitary objects and ultimately assign them to a bin.
 *
 * However, when we're busy bin-packing, we don't really care about those objects
 * identity - we just care about their size. Objects of the same size are
 * fungible as far as bin-packing is concerned.
 */
object BinPacking {

  trait CanFit[T] {
    def canFit(itemSize: T, binSize: T): Boolean
  }

  object CanFit {
    implicit def deriveCanFit[T: Order]: CanFit[T] = new CanFit[T] {
      override def canFit(itemSize: T, binSize: T): Boolean = itemSize <= binSize
    }

    implicit class CanFitOps[T](val a: T)(implicit CF: CanFit[T]) {
      def canFit(item: T): Boolean = CF.canFit(item, a)
    }
  }


  trait Size[T] extends Group[T] with CanFit[T]

  object Size {

    case class CardinalityConstrained(size: Double, cardinality: Double)
    object CardinalityConstrained {
      def item(size: Int) = CardinalityConstrained(size, cardinality = 1)

      implicit val coordSpace: CoordinateSpace[CardinalityConstrained, Double] = new CoordinateSpace[CardinalityConstrained, Double] {

        private val axes = Seq(CardinalityConstrained(1, 0),CardinalityConstrained(0, 1))

        override def dimensions: Int = 2

        override def coord(v: CardinalityConstrained, i: Int): Double = i match {
          case 0 => v.size
          case 1 => v.cardinality
        }

        override def axis(i: Int): CardinalityConstrained = axes(i)

        override implicit def scalar: Field[Double] = spire.implicits.DoubleAlgebra

        override def timesl(r: Double, v: CardinalityConstrained): CardinalityConstrained =
          CardinalityConstrained(r * v.size, r * v.cardinality)

        override def negate(x: CardinalityConstrained): CardinalityConstrained =
          CardinalityConstrained(-x.size, -x.cardinality)

        override val zero: CardinalityConstrained = CardinalityConstrained(0,0)

        override def plus(x: CardinalityConstrained, y: CardinalityConstrained): CardinalityConstrained = CardinalityConstrained(
          x.size + y.size,
          x.cardinality + y.cardinality
        )
      }

      implicit val size: Size[CardinalityConstrained] = deriveSizeFromCoordinateSpace[CardinalityConstrained, Double]

      val canFit: CanFit[CardinalityConstrained] = new CanFit[CardinalityConstrained] {
        override def canFit(itemSize: CardinalityConstrained, binSize: CardinalityConstrained): Boolean =
          itemSize.size <= binSize.size && itemSize.cardinality <= binSize.cardinality
      }
    }

    def apply[T](implicit ev: Size[T]): Size[T] = ev


    implicit def deriveSizeFromCoordinateSpace[V, F: Order](implicit cs: CoordinateSpace[V, F]): Size[V] =
      deriveSize(
        g = cs.additive,
        (itemSize: V, binSize: V) => (0 until cs.dimensions).forall(i => itemSize.coord(i) <= binSize.coord(i))
      )

    implicit def deriveSize[T](implicit g: Group[T], cf: CanFit[T]): Size[T] = new Size[T] {
      override def empty: T = g.empty
      override def combine(x: T, y: T): T = g.combine(x, y)
      override def inverse(a: T): T = g.inverse(a)

      override def canFit(itemSize: T, binSize: T): Boolean = cf.canFit(itemSize, binSize)
    }

    implicit class SizeOps[T](val a: T)(implicit S: Size[T]) {
      def *(n: Int): T =
        if (n == 0) S.empty
        else if (n > 0) S.combineN(a, n)
        else S.inverse(S.combineN(a, -n))
    }
  }

  type Census[T, S] = Map[S, FreqMap[T]]

  object Census {
    def apply[T, S](items: Seq[T], f: T => S): Census[T, S] = FreqMap(items).groupBy(x => f(x._1))
  }

  implicit class RichCensus[T, S](census: Census[T, S]) {
    def removeItems(itemSize: S, quantity: Int): (FreqMap[T], Census[T, S]) = {
      val (extracted, remaining) = census(itemSize).removeItems(quantity)
      (extracted, if (remaining.isEmpty) census.removed(itemSize) else census.updated(itemSize, remaining))
    }

    def sizeFrequencies: FreqMap[S] = census.toMap.mapV(_.totalItems)
  }

  type FreqMap[A] = Map[A, Int]

  implicit class RichFreqMap[T](val input: FreqMap[T]) {
    val totalItems: Int = input.values.sum

    def addOne(item: T): FreqMap[T] =
      input.updatedWith(item)(existingValueOpt => Some(existingValueOpt.getOrElse(0) + 1))

    def removeOne(item: T): FreqMap[T] = {
      val remaining = input(item) - 1
      require(remaining >= 0)
      if (remaining == 0) input.removed(item) else input.updated(item, remaining)
    }

    def removeItems(quantity: Int): (FreqMap[T], FreqMap[T]) = {
      val (item, availableQuantity) = input.head
      val surplus = availableQuantity - quantity

      if (surplus > 0) (Map(item -> quantity), input.updated(item, surplus))
      else if (surplus == 0) (Map(item -> quantity), input.removed(item))
      else {
        val (extracted, remaining) = input.removed(item).removeItems(-surplus)
        (extracted + (item -> availableQuantity), remaining)
      }
    }
  }

  implicit class RichFreqFreqMap[T](input: FreqMap[FreqMap[T]]) {
    def flattenFrequencies: FreqMap[T] = (for {
      (innerFreqMap, frequencyOfInnerMap) <- input.toSeq
      (item, itemInnerFreq) <- innerFreqMap
    } yield item -> (itemInnerFreq * frequencyOfInnerMap)).groupMapReduce(_._1)(_._2)(_ + _)
  }

  object FreqMap {
    def apply[A](s: Seq[A]): FreqMap[A] = s.groupMapReduce(identity)(_ => 1)(_ + _)
  }

  type BinContents[S] = FreqMap[S]

  implicit class RichBinContents[S: Size](m: BinContents[S]) {
    val totalItemSize: S = m.map(x => x._1 * x._2).toSeq.combineAll
  }

  type Packing[S] = FreqMap[BinContents[S]]

  implicit class RichPacking[S](p: Packing[S]) {
    val numBins: Int = p.values.sum
    val itemCounts: FreqMap[S] = p.flattenFrequencies

    val bins: Iterable[BinContents[S]] = for {
      (bc, repsOfBin) <- p
      _ <- 0 until repsOfBin
    } yield bc
  }

  implicit class RichSizePacking[S](p: Packing[S])(implicit S: Size[S]) {
    val totalItemSize: S = p.map(x => x._1.totalItemSize * x._2).toSeq.combineAll
    def largestBinSizeBy[T: Order](f: S => T): Option[T] =
      p.keys.map(bin=> f(bin.totalItemSize)).maxOption(Order[T].toOrdering)
  }

  class ActiveBin[S: Size](var remainingCapacity: S, contents: mutable.HashMap[S, Int]) {
    def addIfFits(item: S): Boolean = {
      val fits = canFit(item)
      if (fits) {
        remainingCapacity = remainingCapacity |-| item
        contents.updateWith(item)(c => Some(c.getOrElse(0) + 1))
      }
      fits
    }

    def canFit(item: S): Boolean = remainingCapacity.canFit(item)

    def toMap: FreqMap[S] = contents.toMap
  }

  object ActiveBin {
    abstract class Selector[S] {
      def select(item: S): BinSelector[S]

      def packOnline(binSize: S, items: Iterator[S])(implicit S: Size[S]): Packing[S] = {
        val bins = mutable.Queue.empty[ActiveBin[S]]
        items.foreach { item =>
          select(item)(bins).fold {
            bins.append(ActiveBin.newBinContaining(binSize, item))
            ()
          } { _.addIfFits(item) }
        }
        FreqMap(bins.toSeq.map(_.toMap))
      }

      def decreasing(implicit o: Order[S], S: Size[S]): OfflineAlgorithm[S] = new OfflineAlgorithm[S] {
        override def pack(binSize: S, itemQuantities: FreqMap[S]): Packing[S] =
          packOnline(binSize, itemsInDecreasingSizeFrom(itemQuantities))
      }
    }

    object Selector {
      type BinSelector[S] = collection.Seq[ActiveBin[S]] => Option[ActiveBin[S]]

      /**
       * [[https://en.wikipedia.org/wiki/First-fit_bin_packing]]
       */
      def firstFit[S: CanFit]: Selector[S] = new Selector[S] {
        override def select(item: S): BinSelector[S] = _.find(_.canFit(item))
      }

      /**
       * [[https://en.wikipedia.org/wiki/Best-fit_bin_packing]]
       */
      def bestFit[S: CanFit: Order]: Selector[S] = new Selector[S] {
        override def select(item: S): BinSelector[S] =
          _.filter(_.canFit(item)).minByOption(_.remainingCapacity)(Order[S].toOrdering)
      }
    }

    def newBinContaining[S: Size](binSize: S, item: S): ActiveBin[S] = {
      val newBin = new ActiveBin[S](binSize, mutable.HashMap.empty)
      newBin.addIfFits(item)
      newBin
    }
  }

  def pack[S: Size: Order](binSize: S, itemQuantities: FreqMap[S], selector: Selector[S]): Packing[S] =
    onlinePack(binSize, itemsInDecreasingSizeFrom(itemQuantities), selector)

  def onlinePack[S: Size](binSize: S, items: Iterator[S], selector: Selector[S]): Packing[S] = {
    val bins = mutable.Queue.empty[ActiveBin[S]]
    items.foreach { item =>
      selector.select(item)(bins).fold {
        bins.append(ActiveBin.newBinContaining(binSize, item))
        ()
      } { _.addIfFits(item) }
    }
    FreqMap(bins.toSeq.map(_.toMap))
  }

  private def itemsInDecreasingSizeFrom[S: Order](itemQuantities: FreqMap[S]): Iterator[S] =
    SortedMap.from(itemQuantities)(Order[S].toOrdering.reverse).iterator.flatMap {
      case (item, quantity) => Iterator.fill(quantity)(item)
    }

}
