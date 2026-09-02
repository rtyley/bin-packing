package com.madgag.algo.packing.binpacking

import cats.data.Ior
import cats.implicits._

case class MapDiff[K, V](
  added: Map[K, V],
  removed: Map[K, V],
  changed: Map[K, (V, V)]
) {
  val isEmpty: Boolean = added.isEmpty && removed.isEmpty && changed.isEmpty
}

object MapDiff {
  def diff[K, V](oldMap: Map[K, V], newMap: Map[K, V]): MapDiff[K, V] = {
    val aligned = oldMap.align(newMap)

    MapDiff(
      added = aligned.collect {
        case (k, Ior.Right(v)) => k -> v
      },
      removed = aligned.collect {
        case (k, Ior.Left(v)) => k -> v
      },
      changed = aligned.collect {
        case (k, Ior.Both(oldV, newV)) if oldV != newV => k -> (oldV, newV)
      }
    )
  }
}
