package com.madgag.algo.packing.binpacking

import cats.Order
import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{bestFit, firstFit}
import com.madgag.algo.packing.binpacking.BinPacking.{FreqMap, Packing, Size}

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
}
