package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector

import scala.collection.immutable.SortedMap
import scala.collection.mutable

object BinPacking {
  type FreqMap[A] = Map[A, Int]

  object FreqMap {
    def apply[A](s: Seq[A]): FreqMap[A] = s.groupMapReduce(identity)(_ => 1)(_ + _)
  }

  type BinContents = FreqMap[Int]
  implicit class RichBinContents(m: BinContents) {
    val totalSize: Int = m.map(x => x._1 * x._2).sum
    def multipliedBy(multiplier: Int): FreqMap[Int] = m.view.mapValues(_ * multiplier).toMap
  }

  type Packing = FreqMap[BinContents]

  implicit class RichPacking(m: Packing) {
    val numBins: Int = m.map(x => x._1.size * x._2).sum
    val totalSize: Int = m.map(x => x._1.totalSize * x._2).sum
    val largestBinSize: Int = m.keys.map(_.totalSize).max
    val itemCounts: FreqMap[Int] = m.toSeq.flatMap(x => x._1.multipliedBy(x._2).toSeq).groupMapReduce(_._1)(_._2)(_ + _)
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
