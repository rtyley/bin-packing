package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.ActiveBin.Selector.{BestFit, FirstFit}
import com.madgag.algo.packing.binpacking.BinPacking.OfflineAlgorithm.FFD
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

  it should "provide sensible syntax and return type for Sets, where items can not be repeated" in {
    val packer = Packer[String](Setup(binCapacity = 7, sizer = _.length), FFD)
    def verify(s: Set[String]): Set[Set[String]] = {
      val packed = s.packWith(packer)
      packed.flatten shouldBe s
      packed.forall(_.map(packer.setup.sizer).sum <= packer.setup.binCapacity) shouldBe true
      packed
    }

    verify(Set("Apple", "an", "Pear", "Fur")) shouldBe Set(Set("Apple", "an"), Set("Pear", "Fur"))

    verify(Set("Foo", "Up", "Bar", "On"))
    verify(Set("Foo", "Up", "Bar", "On", "No", "Go")) // FFD & BFD both use 3 bins here, where 2 would do
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
      val minimumRequiredBins = math.round(math.ceil(itemsFreqMap.totalItemSize.toFloat / binCapacity)).toInt

      packing.numBins should be >= minimumRequiredBins
      val packingBins = packing.numBins
      val badPackingBins = badPacking.numBins
      val ratio = packingBins.toDouble / badPackingBins
      println(ratio)
    }
  }

  "RichFreqMap" should "correctly group items by size" in {
    val collectionAdapter = CollectionAdapter.caFreqMap[String]
    collectionAdapter.censusFor(Map("Foo" -> 5, "Bar" -> 7), _.length) shouldBe Map(3 -> Map("Foo" -> 5, "Bar" -> 7))
  }

  it should "pack things, I guess" in {
    val packer = Packer[String](Setup(binCapacity = 7, sizer = _.length), FFD)

    Map("Foo" -> 3, "Bar" -> 2).packWith(packer).flattenFrequencies shouldBe Map("Foo" -> 3, "Bar" -> 2)
  }

}