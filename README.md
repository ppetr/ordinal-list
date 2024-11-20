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
