# bin-packing

_packing items into bins of fixed sizes, minimising the number of bins used_

https://en.wikipedia.org/wiki/Bin_packing_problem

```scala
val packer: Packer[String] = Packer(Setup(binCapacity = 5, sizer = _.length), FFD)

val packed: Set[Set[String]] = Set("My", "Boom", "Bar", "A").packWith(packer)
// Set(Set("A", "Boom"), Set("My", "Bar"))
```

## See also

Interesting Papers (I haven't implemented these, at least yet)

* [_"Fast Pattern-based Algorithms for Cutting Stock"_](https://research.fdabrandao.pt/papers/cspheuristics.pdf) (2013) on the cutting stock problem (CSP)
  and bin packing problem (BPP).
* [_"Solving Bin Packing Related Problems Using an Arc Flow Formulation"_](https://optimization-online.org/wp-content/uploads/2012/04/3433.pdf) (2012) looks at cardinality-constrained bin packing.

Other interesting algorithms:
* https://github.com/rtyley/k-way-merge

## Alternative libraries

Google's [OR-Tools](https://github.com/google/or-tools) is an open source software suite
for optimization, [written in C++](https://developers.google.com/optimization/introduction/get_started),
but with a [Java wrapper](https://developers.google.com/optimization/introduction/java),
including [support for bin-packing](https://developers.google.com/optimization/pack/bin_packing#java).

* https://github.com/optimatika/ojAlgo - pure Java MIP
* https://github.com/vagmcs/Optimus - Scala, can use ojAlgo or other solvers
