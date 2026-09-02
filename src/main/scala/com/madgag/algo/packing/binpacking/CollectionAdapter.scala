package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking._

private case class CollectionAdapter[T, B[_]](
  freqMapFromColl: B[T] => FreqMap[T],
  collPackingFromPacking: FreqMap[FreqMap[T]] => B[B[T]]
)

private object CollectionAdapter {
  /**
   * For packing when you don't want to repeat any items, which would be normal
   * when you're making API calls about many ids -  no point in asking about the
   * same item twice.
   */
  implicit def caSet[T]: CollectionAdapter[T, Set] =
    CollectionAdapter[T, Set](_.map(_ -> 1).toMap, _.map(_._1.keySet).toSet)

  implicit def caFreqMap[T]: CollectionAdapter[T, FreqMap] =
    CollectionAdapter[T, FreqMap](identity, identity)
}
