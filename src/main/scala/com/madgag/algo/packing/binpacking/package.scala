package com.madgag.algo.packing

package object binpacking {

  /** Adds packing support to any collection for which a
   * [[com.madgag.algo.packing.binpacking.CollectionAdapter]] exists - e.g.
   * `Set` or [[com.madgag.algo.packing.binpacking.BinPacking.FreqMap]]:
   *
   * {{{
   * val packed: Set[Set[String]] =
   *   Set("My", "Boom", "Bar", "A").packWith(packer)
   * }}}
   * */
  implicit class RichCollection[S, B[_]](input: B[S])(implicit ca: CollectionAdapter[S, B]) {
    def packWith(packer: Packer[S]): B[B[S]] = ca.collPackingFromPacking(packer.pack(ca.freqMapFromColl(input)))
  }
}
