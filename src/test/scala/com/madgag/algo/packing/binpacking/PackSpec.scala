package com.madgag.algo.packing.binpacking

case class PackSpec[T](
  binSize: T,
  input: com.madgag.algo.packing.binpacking.BinPacking.FreqMap[T]
)
