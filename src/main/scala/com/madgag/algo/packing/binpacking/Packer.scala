package com.madgag.algo.packing.binpacking

import cats.implicits._
import com.madgag.algo.packing.binpacking.BinPacking._
import com.madgag.algo.packing.binpacking.Packer.Adapter


/**
 * A Packer contains the full definition of how items of arbitrary type A should be packed -
 * the bin-capacity, how to size those items, and what algorithm to do the packing with
 * (eg [[OfflineAlgorithm.FFD]]).
 *
 * By itself, a Packer can pack objects supported by [[OfflineAlgorithm]]. If you want to pack other types of
 * objects - types that can be sized to types supported by [[OfflineAlgorithm]] - you can use the collection
 * support provided by [[RichCollection]].
 */
trait Packer[S] {
  def pack(itemFreqs: FreqMap[S]): Packing[S]

  def using[T](f: T => S): Packer[T] = new Adapter[S, T](this, f)
}

object Packer {

  /**
   * Create a Packer can pack objects supported by the [[OfflineAlgorithm]].
   */
  def apply[S](binSize: S, offlineAlgorithm: OfflineAlgorithm[S]): Packer[S] =
    (itemFreqs: FreqMap[S]) => offlineAlgorithm.pack(binSize, itemFreqs)

  class Adapter[S, T](innerPacker: Packer[S], f: T => S) extends Packer[T] {
    override def pack(itemFreqs: FreqMap[T]): Packing[T] = {
      val census: Census[T, S] = itemFreqs.groupBy(x => f(x._1))

      val ss: Packing[S] = innerPacker.pack(census.sizeFrequencies)
      ss.bins.foldLeft(Adapter.Acc(census))(_ add _).finishedBins
    }
  }

  object Adapter {
    object Acc {
      def apply[T, S](census: Census[T, S]): Acc[T, S] = Acc(census, Map.empty, Map.empty)
    }

    case class Acc[T, S](
      census: Census[T, S],
      finishedBins: Packing[T],
      currentBin: BinContents[T]
    ) {
      def add(binContents: BinContents[S]): Acc[T, S] = binContents.foldLeft(this) {
        case (acc, (item, quantity)) => acc.addToExistingBin(item, quantity)
      }.finishBin

      private def addToExistingBin(itemSize: S, quantity: Int): Acc[T, S] = {
        val (extracted, updatedCensus) = census.removeItems(itemSize, quantity)
        copy(census = updatedCensus, currentBin = currentBin |+| extracted)
      }

      private def finishBin: Acc[T, S] =
        copy(finishedBins = finishedBins |+| Map(currentBin -> 1), currentBin = Map.empty)
    }
  }

}