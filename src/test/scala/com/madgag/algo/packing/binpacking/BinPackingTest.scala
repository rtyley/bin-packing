package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{bestFit, firstFit}
import com.madgag.algo.packing.binpacking.BinPacking._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BinPackingTest extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {

  "BinPacking" should "pack items into bins, using the fewest necessary bins" in {
    val packing: Packing[Int] = BinPacking.pack(binSize = 10, Map(6 -> 1, 5 -> 2, 4 -> 1), firstFit)
    packing.size shouldBe 2
    packing shouldBe Map(
      Map(6 -> 1, 4 -> 1) -> 1,
      Map(5 -> 2) -> 1
    )
    packing.numBins shouldBe 2
  }

  "Packing" should "count the number of bins correctly" in {
    val packing = Map(
      Map(6 -> 1, 4 -> 1) -> 3,
      Map(5 -> 2) -> 17
    )
    packing.numBins shouldBe 20
  }

  val numItemsGen: Gen[(Int, List[Int])] = for {
    binCapacity <- Gen.choose(0, 1000)
    items <- Gen.listOfN(10000, Gen.choose(0, binCapacity))
  } yield (binCapacity, items)

  it should "be cool for lots of stuff" in forAll (numItemsGen) { case (binCapacity, items) =>
    whenever(binCapacity>=0 && items.forall(item => item >= 0 && item <= binCapacity)) {
      val itemsFreqMap: FreqMap[Int] = FreqMap(items)
      val packing: Packing[Int] = BinPacking.pack(binCapacity, itemsFreqMap, bestFit)
      val badPacking: Packing[Int] = BinPacking.pack(binCapacity, itemsFreqMap, firstFit)
      packing.largestBinSizeBy(identity).getOrElse(0) should be <= binCapacity
      packing.itemCounts shouldBe itemsFreqMap
      val minimumRequiredBins = math.round(math.ceil(itemsFreqMap.totalItemSize.toFloat / binCapacity)).toInt

      packing.numBins should be >= minimumRequiredBins
      val packingBins = packing.numBins
      val badPackingBins = badPacking.numBins
      val ratio = packingBins.toDouble / badPackingBins
      println(ratio)
    }
  }

}