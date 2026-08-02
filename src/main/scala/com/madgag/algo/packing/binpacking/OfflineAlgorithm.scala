package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{BestFit, FirstFit}
import com.madgag.algo.packing.binpacking.BinPacking.{FreqMap, Packing}

/**
 * An 'offline' bin-packing algorithm is allowed to see all the items before starting to place
 * them into bins.
 *
 * [[https://en.wikipedia.org/wiki/Bin_packing_problem#Offline_algorithms]]
 */
trait OfflineAlgorithm {
  def pack(binCapacity: Int, itemQuantities: FreqMap[Int]): Packing
}

object OfflineAlgorithm {
  /**
   * [[https://en.wikipedia.org/wiki/First-fit-decreasing_bin_packing]]
   */
  val FFD: OfflineAlgorithm = FirstFit.Decreasing

  val BFD: OfflineAlgorithm = BestFit.Decreasing
}
