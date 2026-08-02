package com.madgag.algo.packing.binpacking

import cats.Monoid
import cats.syntax.all._
import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector
import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{BestFit, FirstFit}
import com.madgag.algo.packing.binpacking.BinPacking.CollectionAdapter.Acc
import com.madgag.scala.collection.decorators._

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

  abstract class CollectionAdapter[T, B[_]](implicit val mb: Monoid[B[T]], val mbb: Monoid[B[B[T]]] ) {
    val emptyBin: B[T] = mb.empty

    def accFor(census: Census[T]): Acc[T, B] = Acc(census, mbb.empty, emptyBin, this)
    def censusFor(input: B[T], sizer: T => Int): Census[T]
    def addableToCurrentBin(extracted: FreqMap[T]): B[T]
    def addableToFinishedBins(currentBin: B[T]): B[B[T]]
  }

  object CollectionAdapter {
    /**
     * For packing when you don't want to repeat any items, which would be normal
     * when you're making API calls about many ids -  no point in asking about the
     * same item twice.
     */
    implicit def caSet[T]: CollectionAdapter[T, Set] = new CollectionAdapter[T, Set] {
      def censusFor(input: Set[T], sizer: T => Int): Census[T] = input.groupUp(sizer)(_.map(_ -> 1).toMap)
      def addableToCurrentBin(extracted: FreqMap[T]): Set[T] = extracted.keySet
      def addableToFinishedBins(currentBin: Set[T]): Set[Set[T]] = Set(currentBin)
    }

    implicit def caFreqMap[T]: CollectionAdapter[T, FreqMap] = new CollectionAdapter[T, FreqMap] {
      def censusFor(input: FreqMap[T], sizer: T => Int): Census[T] = input.groupBy(x => sizer(x._1))
      def addableToCurrentBin(extracted: FreqMap[T]): FreqMap[T] = extracted
      def addableToFinishedBins(currentBin: FreqMap[T]): FreqMap[FreqMap[T]] = Map(currentBin -> 1)
    }

    case class Acc[T, B[_]](
      census: Census[T],
      finishedBins: B[B[T]],
      currentBin: B[T],
      collectionAdapter: CollectionAdapter[T, B]
    ) {
      import collectionAdapter._

      def add(binContents: BinContents): Acc[T, B] = binContents.foldLeft(this) {
        case (acc, (item, quantity)) => acc.addToExistingBin(item, quantity)
      }.finishBin

      def addToExistingBin(itemSize: Int, quantity: Int): Acc[T, B] = {
        val (extracted, updatedCensus) = census.removeItems(itemSize, quantity)
        copy(
          census = updatedCensus,
          currentBin = currentBin |+| addableToCurrentBin(extracted)
        )
      }

      def finishBin: Acc[T, B] =
        copy(finishedBins = finishedBins |+| addableToFinishedBins(currentBin), currentBin = emptyBin)
    }
  }

  type Census[A] = Map[Int, FreqMap[A]]

  object Census {
    def apply[A](items: Seq[A], f: A => Int): Census[A] = FreqMap(items).groupBy(x => f(x._1))
  }

  implicit class RichCensus[T](census: Census[T]) {
    def removeItems(itemSize: Int, quantity: Int): (FreqMap[T], Census[T]) = {
      val (extracted, remaining) = census(itemSize).removeItems(quantity)
      (extracted, if (remaining.isEmpty) census.removed(itemSize) else census.updated(itemSize, remaining))
    }

    def itemQuantities: FreqMap[Int] = census.mapV(_.totalItems)
  }

  type FreqMap[A] = Map[A, Int]

  implicit class RichFreqMap[T](val input: FreqMap[T]) {
    val totalItems: Int = input.values.sum

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

  type BinContents = FreqMap[Int]
  implicit class RichBinContents(m: BinContents) {
    val totalItemSize: Int = m.map(x => x._1 * x._2).sum
  }

  type Packing = FreqMap[BinContents]

  implicit class RichPacking(p: Packing) {
    val numBins: Int = p.map(x => x._1.size * x._2).sum
    val totalItemSize: Int = p.map(x => x._1.totalItemSize * x._2).sum
    val largestBinSize: Int = if (p.isEmpty) 0 else p.keys.map(_.totalItemSize).max
    val itemCounts: FreqMap[Int] = p.flattenFrequencies

    val bins: Iterable[BinContents] = for {
      (bc, repsOfBin) <- p
      _ <- 0 until repsOfBin
    } yield bc
  }

  val reverseIntOrdering: Ordering[Int] = implicitly[Ordering[Int]].reverse

  class ActiveBin(var remainingCapacity: Int, contents: mutable.HashMap[Int, Int] = mutable.HashMap.empty) {
    def addIfFits(item: Int): Boolean = {
      val canFitItem = remainingCapacity >= item
      if (canFitItem) {
        remainingCapacity -= item
        contents.updateWith(item)(c => Some(c.getOrElse(0) + 1))
      }
      canFitItem
    }

    def toMap: FreqMap[Int] = contents.toMap
  }

  object ActiveBin {
    trait Selector {
      def select(item: Int): collection.Seq[ActiveBin] => Option[ActiveBin]

      def packOnline(binCapacity: Int, items: Iterator[Int]): Packing = {
        val bins = mutable.Queue.empty[ActiveBin]
        items.foreach { item =>
          select(item)(bins).fold {
            bins.append(ActiveBin.newBinContaining(binCapacity, item))
            ()
          } { _.addIfFits(item) }
        }
        FreqMap(bins.toSeq.map(_.toMap))
      }

      val Decreasing: OfflineAlgorithm = (binCapacity: Int, itemQuantities: FreqMap[Int]) =>
        packOnline(binCapacity, itemsInDecreasingSizeFrom(itemQuantities))
    }

    object Selector {
      /**
       * [[https://en.wikipedia.org/wiki/First-fit_bin_packing]]
       */
      case object FirstFit extends Selector {
        override def select(item: Int) = _.find(_.remainingCapacity >= item)
      }

      /**
       * [[https://en.wikipedia.org/wiki/Best-fit_bin_packing]]
       */
      case object BestFit extends Selector {
        override def select(item: Int) = _.filter(_.remainingCapacity >= item).minByOption(_.remainingCapacity)
      }
    }

    def newBinContaining(binCapacity: Int, item: Int): ActiveBin = {
      val newBin = new ActiveBin(binCapacity)
      newBin.addIfFits(item)
      newBin
    }
  }

  def pack(binCapacity: Int, itemQuantities: FreqMap[Int], selector: Selector): Packing =
    onlinePack(binCapacity, itemsInDecreasingSizeFrom(itemQuantities), selector)

  def onlinePack(binCapacity: Int, items: Iterator[Int], selector: Selector): Packing = {
    val bins = mutable.Queue.empty[ActiveBin]
    items.foreach { item =>
      selector.select(item)(bins).fold {
        bins.append(ActiveBin.newBinContaining(binCapacity, item))
        ()
      } { _.addIfFits(item) }
    }
    FreqMap(bins.toSeq.map(_.toMap))
  }

  private def itemsInDecreasingSizeFrom(itemQuantities: FreqMap[Int]): Iterator[Int] =
    SortedMap.from(itemQuantities)(reverseIntOrdering).iterator.flatMap {
      case (item, quantity) => Iterator.fill(quantity)(item)
    }

}
