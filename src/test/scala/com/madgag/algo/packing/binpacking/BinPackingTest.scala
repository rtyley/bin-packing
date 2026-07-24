package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{BestFit, FirstFit}
import com.madgag.algo.packing.binpacking.BinPacking._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BinPackingTest extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {
  "BinPacking" should "pack items into bins, using the fewest necessary bins" in {

    val bins: Map[BinContents, Int] = BinPacking.pack(binCapacity = 10, Map(6 -> 1, 5 -> 2, 4 -> 1), FirstFit)
    bins.size shouldBe 2
    bins shouldBe Map(
      Map(6 -> 1, 4 -> 1) -> 1,
      Map(5 -> 2) -> 1
    )
  }

  val numItemsGen: Gen[(Int, List[Int])] = for {
    binCapacity <- Gen.choose(0, 1000)
    items <- Gen.listOfN(10000, Gen.choose(0, binCapacity))
  } yield (binCapacity, items)

  it should "be cool for lots of stuff" in forAll (numItemsGen) { case (binCapacity, items) =>
    whenever(binCapacity>=0 && items.forall(item => item >= 0 && item <= binCapacity)) {
      val itemsFreqMap: FreqMap[Int] = FreqMap(items)
      val packing: Packing = BinPacking.pack(binCapacity, itemsFreqMap, BestFit)
      val badPacking: Packing = BinPacking.pack(binCapacity, itemsFreqMap, FirstFit)
      packing.largestBinSize should be <= binCapacity
      packing.itemCounts shouldBe itemsFreqMap
      val minimumRequiredBins = math.round(math.ceil(itemsFreqMap.totalSize.toFloat / binCapacity)).toInt

      packing.numBins should be >= minimumRequiredBins
      val packingBins = packing.numBins
      val badPackingBins = badPacking.numBins
      val ratio = packingBins.toDouble / badPackingBins
      println(ratio)
    }
  }
}