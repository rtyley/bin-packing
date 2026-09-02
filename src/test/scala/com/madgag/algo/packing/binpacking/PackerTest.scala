package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.CanFit.CanFitOps
import com.madgag.algo.packing.binpacking.BinPacking.Packing
import com.madgag.algo.packing.binpacking.OfflineAlgorithm.{FFD, dotProductFFD}
import org.scalacheck.util.FreqMap
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import spire.algebra.CoordinateSpace
import spire.implicits._
import BinPacking._
import com.madgag.algo.packing.binpacking.BinPacking.Size.CardinalityConstrained




class PackerTest extends AnyFlatSpec with Matchers with Inspectors {

  val doublePacker: Packer[Double] = Packer(134.5, FFD)
  private val vec2binSize = Array[Double](20, 8)
  val vec2packer: Packer[Array[Double]] =
    Packer(vec2binSize, dotProductFFD(CoordinateSpace.array[Double](2)))

  "A Packer" should "be able to do simple bag-like packing of any type supported by an OfflineAlgorithm" in {
    doublePacker.pack(Map(121.5d -> 2, 13d -> 2)) shouldBe Map(Map(121.5d -> 1, 13d -> 1) -> 2)

    val v1 = Array(18d, 1d)
    val v2 = Array(1d, 7d)
    vec2packer.pack(Map(v1 -> 2, v2 -> 2)) shouldEqual Map(Map(v1 -> 1, v2 -> 1) -> 2)
  }

  it should "handle Cardinality-constrained packing" in {
    val packSpec = PackSpec(
      CardinalityConstrained(10, 4),
      Map(CardinalityConstrained.item(3) -> 2, CardinalityConstrained.item(7) -> 2, CardinalityConstrained.item(5) -> 3)
    )
    val packer: Packer[CardinalityConstrained] =
      Packer(packSpec.binSize, dotProductFFD(CardinalityConstrained.coordSpace))

    checkPacking(packSpec, packer.pack(packSpec.input))
  }

  it should "be able to convert from arbitrary types to types supported by OfflineAlgorithm" in {
    val stringPacker: Packer[String] = Packer[Int](binSize = 5, FFD).using[String](_.length)

    stringPacker.pack(Map("My" -> 1, "Boom" -> 1, "Bar" -> 1, "A" -> 1)) shouldBe
      Map(Map("A" -> 1, "Boom" -> 1) -> 1, Map("My" -> 1, "Bar" -> 1) -> 1)
  }

  it should "not hang if items are too big for packer" in {
    val stringPacker: Packer[String] = Packer[Int](binSize = 3, FFD).using(_.length)

    stringPacker.pack(Map("Foo" -> 1, "Bar" -> 1, "Boom" -> 1, "A" -> 1))
  }

  /**
   * General rules for packers
   *
   * - do not hang if items are too big - but should packing partition-out the items that are too big and return them?
   *   Or just die?
   * - Once packed:
   *   - the number of each type of item should remain the same
   *   - the number of items should remain the same / there should be no new items!
   *   - each bin should respect the bin size limit
   *   - if the input is empty, there should be no output bins - just Map.empty()
   *   - no bin should be empty
   *
   * Packing quality:
   * - It should not be possible to combine any two bins and have them stay under the bin size limit?
   * - Should not produce a packing worse than worst-case bound??
   *   - FFD(I) ≤ 11/9 OPT(I) + 6/9 https://en.wikipedia.org/wiki/First-fit-decreasing_bin_packing#cite_ref-Dosa07_5-0
   *   - Nice-to-have: a way to produce or recall optimal packings.
   *     - Take an arrangement of full bins, optionally with one additional partially-full bin.
   *       Then partition those bins at will, in as many different ways as you like.
   *       We know that the optimal packing for the produced items requires the number of bins we started with
   *
   * If multiple algorithms can pack the same kind of item, it would be good to compare their packing - eg number of
   * bins used. Even runtime (hah!)
   */
  def checkPacking[T: Size](
    packSpec: PackSpec[T],
    output: Packing[T]
  ): Unit = {
    val input = packSpec.input
    if (input.isEmpty) output shouldBe empty else {
      MapDiff.diff(input, output.itemCounts) shouldBe empty
      val bins = output.keySet
      val binSize = packSpec.binSize
      val remainingSpace = bins.map(bin => bin -> (binSize |-| bin.totalItemSize)).toMap
      forAll(bins) { bin =>
        bin should not be empty
        val consumedSize = bin.totalItemSize
        binSize.canFit(consumedSize) shouldBe true
        val otherBins = remainingSpace.removed(bin)
        otherBins.values.exists(_.canFit(consumedSize)) shouldBe false
      }
    }
  }
}