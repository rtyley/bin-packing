package com.madgag.algo.packing.binpacking

import com.madgag.algo.packing.binpacking.BinPacking._
import com.madgag.algo.packing.binpacking.OfflineAlgorithm.FFD
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RichCollectionTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  val packer: Packer[String] = Packer(Setup(binCapacity = 5, sizer = _.length), FFD)

  val itemsThatCanFitInBinGen: Gen[String] =
    Gen.choose(0, packer.setup.binCapacity).flatMap(size => Gen.stringOfN(size, Gen.alphaLowerChar))

  val itemSetsGen: Gen[Set[String]] = Gen.containerOf[Set, String](itemsThatCanFitInBinGen)

  "Set" should "have a nice 'packWith' syntax" in {
    Set("My", "Boom", "Bar", "A").packWith(packer) shouldBe Set(Set("A", "Boom"), Set("My", "Bar"))
  }

  it should "work for many test samples" in forAll (itemSetsGen) { items =>
    val packedSets: Set[Set[String]] = items.packWith(packer)
    packedSets.flatten shouldBe items
    packedSets.forall(_.toSeq.map(_.length).sum <= packer.setup.binCapacity)
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
    val packedSets: FreqMap[FreqMap[String]] = items.packWith(packer)
    packedSets.flattenFrequencies shouldBe items
    packedSets.forall(_._1.map(x => x._1.length * x._2).sum <= packer.setup.binCapacity)
  }
}
