---
date: 2024-01-25 02:59:41.358296-07:00
description: "Complex numbers, consisting of a real and imaginary part, are essential\
  \ in various computational fields like engineering, physics, and signal processing.\u2026"
lastmod: 2024-02-19 22:05:18.586635
model: gpt-4-1106-preview
summary: "Complex numbers, consisting of a real and imaginary part, are essential\
  \ in various computational fields like engineering, physics, and signal processing.\u2026"
title: Working with complex numbers
---

{{< edit_this_page >}}

## What & Why?

Complex numbers, consisting of a real and imaginary part, are essential in various computational fields like engineering, physics, and signal processing. Programmers use them to solve equations that real numbers can't, like finding the roots of negative numbers.

## How to:

Haskell handles complex numbers with the `Data.Complex` module. Here's a quick tour:

```haskell
import Data.Complex

-- Define two complex numbers
let z1 = 3 :+ 4  -- that's 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Arithmetic operations
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Complex conjugate
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- Magnitude and phase
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- Polar to rectangular conversion and vice versa
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- same as z1
```

Sample output after loading the above code in GHCi might be:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## Deep Dive

Complex numbers date back to the 16th century but were widely accepted much later. Haskell, like many languages, provides native support for complex arithmetic, making it easy to work with these numbers without implementing the underlying math.

Alternatives include building your custom complex number type or using libraries for specific domains such as quaternions for 3D graphics. But for most use cases, Haskell's `Data.Complex` is plenty.

Under the hood, `Data.Complex` is just a data type pairing two `Float` or `Double` values, representing the real and imaginary parts, respectively. It's a straightforward and efficient way to work with complex numbers on the Haskell platform.

## See Also

Check out these resources for more on complex numbers in Haskell:

- The official Haskell `Data.Complex` documentation: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- A deeper dive into Haskell's number types: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- For an application, explore Fast Fourier Transform algorithms in Haskell: [Haskell FFT library](https://hackage.haskell.org/package/fft)
