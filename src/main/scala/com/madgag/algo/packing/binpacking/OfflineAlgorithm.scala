package com.madgag.algo.packing.binpacking

import cats.data.NonEmptySeq
import cats.implicits.toReducibleOps
import cats.{Id, Monad, Order}
import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{bestFit, firstFit}
import com.madgag.algo.packing.binpacking.BinPacking.CanFit.CanFitOps
import com.madgag.algo.packing.binpacking.BinPacking.Size.deriveSizeFromCoordinateSpace
import com.madgag.algo.packing.binpacking.BinPacking.{CanFit, FreqMap, Packing, Size}
import spire.algebra.{CoordinateSpace, Field}
//import cats.implicits._
import com.madgag.algo.packing.binpacking.BinPacking.RichFreqMap
import com.madgag.algo.packing.binpacking.VectorBinPacking.avdem
import spire.implicits._

/**
 * An 'offline' bin-packing algorithm is allowed to see all the items before starting to place
 * them into bins.
 *
 * [[https://en.wikipedia.org/wiki/Bin_packing_problem#Offline_algorithms]]
 */
trait OfflineAlgorithm[S] {
  def pack(binSize: S, itemQuantities: FreqMap[S]): Packing[S]
}

object OfflineAlgorithm {
  /**
   * [[https://en.wikipedia.org/wiki/First-fit-decreasing_bin_packing]]
   */
  def FFD[S: Size: Order]: OfflineAlgorithm[S] = firstFit.decreasing

  def BFD[S: Size: Order]: OfflineAlgorithm[S] = bestFit.decreasing

  def dotProductFFD[S, F: Order: Field](space: CoordinateSpace[S, F]): OfflineAlgorithm[S] = new OfflineAlgorithm[S] {
    implicit val s: CoordinateSpace[S, F] = space
    implicit val size: Size[S] = deriveSizeFromCoordinateSpace[S, F]

    override def pack(binSize: S, itemQuantities: FreqMap[S]): Packing[S] = if (itemQuantities.isEmpty) Map.empty else {
      val result = fillANewBin(binSize, itemQuantities)
      pack(binSize, result.remainingSupply).addOne(result.bin)
    }

    def weightedRemainingCapacityFor(binState: BinFittingState[S]): S = {
      val av = avdem[S, F](binState.remainingSupply)
      val ss: Seq[S] = (0 until space.dimensions).map { i =>
        val scalar: F = binState.fillingBin.remainingCapacity.coord(i) * av.coord(i)
        scalar *: space.axis(i)
      }
      ss.qsum
    }

    def fillANewBin(binSize: S, itemQuantities: FreqMap[S]): BinFittingResult[S] =
      Monad[Id].tailRecM(BinFittingState[S](FillingBin(initialCapacity = binSize), remainingSupply = itemQuantities)) {
        binState =>
          binState.remainingItemsThatFit.map { itemsThatFit =>
            val remainingCapacity = weightedRemainingCapacityFor(binState)
            binState.enBin(itemsThatFit.maximumBy(_ dot remainingCapacity))
          }.toLeft(binState.asResult)
      }
  }

  case class FillingBin[S: Size](bin: FreqMap[S], remainingCapacity: S) {
    def placeInBin(item: S): FillingBin[S] = copy(
      bin = bin.addOne(item),
      remainingCapacity = remainingCapacity |-| item
    )
  }

  object FillingBin {
    def apply[S: Size](initialCapacity: S): FillingBin[S] = FillingBin(Map.empty, initialCapacity)
  }

  case class BinFittingState[S: CanFit](fillingBin: FillingBin[S], remainingSupply: FreqMap[S]) {
    lazy val remainingItemsThatFit: Option[NonEmptySeq[S]] =
      NonEmptySeq.fromSeq(remainingSupply.keys.filter(fillingBin.remainingCapacity.canFit).toSeq)

    def enBin(item: S): BinFittingState[S] = copy(
      fillingBin = fillingBin.placeInBin(item),
      remainingSupply = remainingSupply.removeOne(item)
    )

    lazy val asResult: BinFittingResult[S] = BinFittingResult(fillingBin.bin, remainingSupply)
  }

  case class BinFittingResult[S](bin: FreqMap[S], remainingSupply: FreqMap[S])
}
