package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.{FreqMap, _}
import spire.algebra.{Field, VectorSpace}
import spire.syntax.all._

/**
 * From "Heuristics for Vector Bin Packing":
 *
 * In the d-dimensional Vector Bin Packing Problem (VBP_d) we are given a set
 * ℐ of n items I¹, I², ..., Iⁿ where each Iⁱ ∈ ℝᵈ. A ''valid packing'' is a partition of
 * ℐ into k sets (or bins) B₁, ..., Bₖ where for each bin j and for each dimension i,
 * ∑_{ℓ ∈ Bⱼ} Iᵢˡ ≤ 1. The goal in the VBP_d problem is to find a valid packing while
 * minimizing k.
 *
 * Dot-Product (DotProduct): This heuristic defines “largest” to be the item that
 * maximises the dot product between the vector of remaining capacities and the
 * vector of demands for the item. Formally, at time `t` let `r(t)` denote the vector
 * of ''remaining'' or ''residual'' capacities of the current open bin, i.e. subtract from
 * the bin’s capacity the total demand of all the items currently assigned to it.
 *
 * It places the item `Iˡ` that maximises the a-weighted dot product `∑ᵢ aᵢIˡᵢr(t)ᵢ`
 * with the vector of remaining capacities without violating the capacity constraint.
 * The weights `aᵢ` are calculated in the same manner as described in Section 2.
 *
 * A natural choice is to take aᵢ to be equal to the average demand
 * `avdemᵢ = ¹⁄ₙ ∑ₗ₌₁ⁿ Iˡᵢ`in dimension `i` (''AvgSum'').
 *
 * Another option is to take `aᵢ` to be exponential in `avdemᵢ`, i.e.
 * `aᵢ = exp(ε · avdemᵢ)` for some suitable constant `ε`  (''ExpSum'').
 */
object VectorBinPacking {
  def avdem[V, F](vectors: Iterable[V])(implicit V: VectorSpace[V, F]): V =
    vectors.qsum :/ vectors.size

  def avdem[V, F: Field](bag: FreqMap[V])(implicit V: VectorSpace[V, F]): V =
    bag.map { case (item, quant) => item :* quant }.qsum :/ bag.totalItems
}
