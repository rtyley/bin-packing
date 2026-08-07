package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.{FreqMap, Packing}

case class Setup[A, S](binSize: S, sizer: A => S) {
  def using(offlineAlgorithm: OfflineAlgorithm[S]) = Packer(this, offlineAlgorithm)
}

/**
 * A Packer contains the full definition of how items of arbitrary type A should be packed -
 * the bin-capacity, how to size those items, and what algorithm to do the packing with
 * (eg [[OfflineAlgorithm.FFD]]).
 *
 * By itself, a Packer can pack integers. If you want to pack some other items - items
 * that can be sized to integers - you can use the collection support provided by
 * [[RichCollection]].
 */
case class Packer[A, S](setup: Setup[A, S], offlineAlgorithm: OfflineAlgorithm[S]) {
  def pack(bareItemFrequenciesBySize: FreqMap[S]): Packing[S] =
    offlineAlgorithm.pack(setup.binSize, bareItemFrequenciesBySize)
}
