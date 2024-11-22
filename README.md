# Infinite lists indexed by [ordinals][1] up to ω<sup>ω</sup>

[1]: https://en.wikipedia.org/wiki/Ordinal_number

_Status: Working implementation._

_Disclaimer: This is not an official Google product._

## Purpose

In some languages (like Haskell) it is possible to have infinite lists
(sometimes also called _streams_). This experimental library extends the
concept to infinite lists indexed by ordinals beyond natural numbers, up to
ω<sup>ω</sup>.

![Illustration of ω<sup>ω</sup>](https://upload.wikimedia.org/wikipedia/commons/a/af/Omega-exp-omega-normal-dark_svg.svg)

Preserving the ordinal structure allows to examine even infinite portions of
such lists. For example, with Haskell's regular [infinite lists][infinite] `xs
++ ys` the `ys` part is forever lost (while still being kept in memory), there
is no way to access it within a finite number of steps, or equivalently
indexing the list with a natural number. But represented as an ordinal list
(corresponding to _ω+ω_), it is possible to retrieve both infinite parts.

[infinite]: https://en.wikibooks.org/wiki/Haskell/Lists_II#Infinite_Lists

## Examples

A finite list is also an ordinal list:
```haskell
ghci> fromFinite [1,2,3] :: OList Integer
<1,2,3>
```
Streams, infinite lists, are also ordinal lists:
```haskell
ghci> omega = fromStream (S.iterate (+ 1) 0) :: OList Integer
ghci> omega
<<[0,1,2,3,...]>>
```
Note however that `fromFinite` won't accept an infinite `[a]` (it'll diverge).
The reason is that the structure - finite- or infinite-ness must be known when
constructing an ordinal list.

Infinite ordinal lists can be concatenated and both parts are still available:
```haskell
ghci> omega <> omega
<<[0,1,2,3,...],[0,1,2,3,...]>>
```
Concatenation also works with finite lists:
```haskell
ghci> fromFinite [42,73] <> omega <> omega
<<[42,73,0,1,...],[0,1,2,3,...]>>
ghci> omega <> omega <> fromFinite [42, 73]
<<[0,1,2,3,...],[0,1,2,3,...]>,42,73>
```
Furthermore, ordinal lists can be multiplied together using the `Applicative`
interface.
```haskell
ghci> (*) <$> omega <*> fromFinite [1, -1]
<<[0,1,2,3,...],[0,-1,-2,-3,...]>>
ghci> (*) <$> fromFinite [1, -1] <*> omega
<<[0,0,1,-1,...]>>
ghci> (,) <$> omega <*> omega
-- Indented for better readability:
<<<[[(0,0),(1,0),(2,0),(3,0),...],
    [(0,1),(1,1),(2,1),(3,1),...],
    [(0,2),(1,2),(2,2),(3,2),...],
    [(0,3),(1,3),(2,3),(3,3),...],...]>>>
ghci> let o = omega <> fromFinite [-1] in (,) <$> o <*> o
<<<[[(0,0),(1,0),(2,0),(3,0),...],
    [(-1,0),(0,1),(1,1),(2,1),...],
    [(-1,1),(0,2),(1,2),(2,2),...],
        [(-1,2),(0,3),(1,3),(2,3),...],...]>,
   [(0,-1),(1,-1),(2,-1),(3,-1),...]>,(-1,-1)>
```
Ordinals also can be compared, subtracted and zipped together. Function
`zipSplit` implements all these operations in one:
```haskell
data N.OListOrdering a b
  = N.OListLT (OList1 b) | N.OListEQ | N.OListGT (OList1 a)

zipSplit :: OList a -> OList b -> (OList (a, b), OListOrdering a b)
```
The first result holds the ordinals zipped together (with the length of the
smaller ordinal), while the second result holds the result of the comparison
together with the result of the subtraction.
```haskell
ghci> zipSplit (omega <> fmap (`subtract` 0) omega) (namedOrdinals S.!! 2)
(<<[(0,"0"),(1,"1"),(2,"2"),(3,"3"),...],
   [(0,"ω"),(-1,"ω+1"),(-2,"ω+2"),(-3,"ω+3"),...]>>,
 OListLT <<<[["2ω","2ω+1","2ω+2","2ω+3",...],
             ["3ω","3ω+1","3ω+2","3ω+3",...],
             ["4ω","4ω+1","4ω+2","4ω+3",...],
             ["5ω","5ω+1","5ω+2","5ω+3",...],...]>>>)
```
where `namedOrdinals` is a `Stream (OList String)` of ordinals, _i_-th listing
a human representation of ordinals up to ω<sup>i</sup>.

## Contributions and future plans

Contributions welcome, please see [Code of Conduct](docs/code-of-conduct.md)
and [Contributing](docs/contributing.md).

See "TODO" comments in the code.

At the moment ordinal lists are planned to support only concatenation
(corresponding to [ordinal addition][operation]) and products (corresponding
to [ordinal multiplication][operation]). Once these operations are finished,
it might be possible to consider extending the concept to ordinal
exponentiation as well, to lists indexed by ordinals up to [ε₀][epsilon]. In
case of interest please open a feature request on the project's GitHub page.

[operation]: https://en.wikipedia.org/wiki/Ordinal_arithmetic
[epsilon]: https://en.wikipedia.org/wiki/Epsilon_numbers_(mathematics)
