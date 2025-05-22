# mutable-fenwick

![Hackage Version](https://img.shields.io/hackage/v/mutable-fenwick?color=blue)

This package provides an implementation of mutable
[Fenwick trees](https://en.wikipedia.org/wiki/Fenwick_tree) in Haskell.

## Features

It is maximally generic. Each operation of Fenwick tree is implemented using a subset of constraints
from `Semigroup`, `Monoid`, or `Commutative`, chosen carefully based on the nature of each
operation. This is mostly possible due to how Haskell typeclasses work, and provides different
functionality based on the constraints provided by the underlying element type.

It is fast and efficient. Every operation is marked as inline, meaning that they will be optimized
for the given element type. With `ArrayC` and `VectorC` from this package, it is possible to use
unboxed arrays and vectors for newtypes that implement a custom algebra (e.g. `Sum`, `Product` or
`Xor`). An implementation using this library can be as fast as a C/C++ implementation.

It is the only Haskell library (I believe, as of this date) that provides Fenwick trees. 
- The [FenwickTree](https://hackage.haskell.org/package/FenwickTree) package is more similar to a
  Segment tree, and it does not have a generic interface for the data structure.
- The [binary-indexed-tree](https://hackage.haskell.org/package/binary-indexed-tree) package
  has an interface for ST monad, but the implementation is only limited to `Sum` monoid.
