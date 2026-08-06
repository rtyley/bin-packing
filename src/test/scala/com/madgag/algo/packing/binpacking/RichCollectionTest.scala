package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking.Size.CardinalityConstrained
import com.madgag.algo.packing.binpacking.BinPacking._
import com.madgag.algo.packing.binpacking.OfflineAlgorithm.FFD
import org.scalacheck.Gen
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RichCollectionTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  val packer: Packer[String, Int] = Packer(Setup(binSize = 5, sizer = _.length), FFD)
  val ccPacker: Packer[String, CardinalityConstrained] =
    Packer(Setup(binSize = CardinalityConstrained(size = 5, cardinality = 3), sizer = str => CardinalityConstrained.item(size = str.length)), FFD)

  val itemsThatCanFitInBinGen: Gen[String] =
    Gen.choose(0, packer.setup.binSize).flatMap(size => Gen.stringOfN(size, Gen.alphaLowerChar))

  val itemSetsGen: Gen[Set[String]] = Gen.containerOf[Set, String](itemsThatCanFitInBinGen)

  "Set" should "have a nice 'packWith' syntax" in {
    Set("My", "Boom", "Bar", "A").packWith(packer) shouldBe Set(Set("A", "Boom"), Set("My", "Bar"))
  }

  it should "work for many test samples" in forAll (itemSetsGen) { items =>
    val packedSets: Set[Set[String]] = items.packWith(packer)
    packedSets.flatten shouldBe items
    Inspectors.forAll(packedSets) { _.toSeq.map(_.length).sum should be <= packer.setup.binSize }
  }

  "Packing Sets while also respecting cardinality constraints" should "work" in {
    Set("A", "B", "C", "D").packWith(ccPacker).size shouldBe 2
  }

  it should "for a single item that is the size of the bin" in {
    Set("ABCDE").packWith(ccPacker) shouldBe Set(Set("ABCDE"))
  }

  it should "work for many test samples" in forAll (itemSetsGen) { items =>
    val packedSets: Set[Set[String]] = items.packWith(ccPacker)
    packedSets.flatten shouldBe items

    val binSize = ccPacker.setup.binSize
    Inspectors.forAll(packedSets) { set =>
      set.toSeq.map(_.length).sum should be <= binSize.size
      set.size should be <= binSize.cardinality
    }
  }

  "FreqMap" should "have a nice 'packWith' syntax" in {
    val freqMap: FreqMap[String] = Map("A" -> 2, "BC" -> 4, "Fiver" -> 1)
    freqMap.packWith(packer) shouldBe Map(Map("A" -> 1, "BC" -> 2) -> 2, Map("Fiver" -> 1) -> 1)
  }

  val itemFreqMapsGen: Gen[FreqMap[String]] = Gen.nonEmptyMap(for {
    item <- itemsThatCanFitInBinGen
    quant <- Gen.choose(1, 10)
  } yield item -> quant)

  it should "work for many test samples" in forAll (itemFreqMapsGen) { items: FreqMap[String] =>
    val packedBags: FreqMap[FreqMap[String]] = items.packWith(packer)
    packedBags.flattenFrequencies shouldBe items

    Inspectors.forAll(packedBags) { case (bag, _) =>
      bag.map(x => x._1.length * x._2).sum should be <= packer.setup.binSize
    }
  }
}
