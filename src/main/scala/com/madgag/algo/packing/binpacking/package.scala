package com.madgag.algo.packing

import com.madgag.algo.packing.binpacking.BinPacking._

package object binpacking {

  /** Adds packing support to any collection for which a
   * [[com.madgag.algo.packing.binpacking.BinPacking.CollectionAdapter]] exists - e.g.
   * `Set` or [[com.madgag.algo.packing.binpacking.BinPacking.FreqMap]]:
   *
   * {{{
   * val packed: Set[Set[String]] =
   *   Set("My", "Boom", "Bar", "A").packWith(packer)
   * }}}
   * */
  implicit class RichCollection[T, B[_]](input: B[T])(implicit ca: CollectionAdapter[T, B]) {
    def packWith[S: Size](packer: Packer[T, S]): B[B[T]] = {
      val census = ca.censusFor(input, packer.setup.sizer)
      packer.pack(census.sizeFrequencies).bins.foldLeft(ca.accFor(census))(_ add _).finishedBins
    }
  }
}
