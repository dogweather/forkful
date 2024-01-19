---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Haskell and the Art of Random Number Generation

## What & Why?

Generating random numbers is about producing non-deterministic values. Programmers use it to simulate scenarios, shuffle elements, create unique IDs, and more.

## How to:

The most basic way to generate random numbers in Haskell involves using the `randomRIO` function from the `System.Random` library.

```Haskell
import System.Random

generateRandomInt :: IO Int
generateRandomInt = randomRIO (1, 100)
```

Running this `generateRandomInt` function gives you random integers between 1 and 100. Each time you call it, you're getting a fresh random value.

```Haskell
> generateRandomInt
42
> generateRandomInt
67
> generateRandomInt
13
```

## Deep Dive:

Haskell's `System.Random` library is based on the algorithms from Donald E. Knuth's "The Art of Computer Programming, Volume 2", specifically section 3.2.1, 3.2.2, and 3.3.4. It's a classic approach, but modern alternative libraries like `random-fu` and `mwc-random` offer better performance and reliability.

The key concept here is that these random numbers are actually pseudo-random; they are deterministic but emulate randomness. For genuine randomness, we'd need external sources, like system entropy or hardware devices.

Did you notice `IO` in `IO Int`? It's because random number generation is an effectful operation; it has side-effects (each call can produce a different result). This is why it's wrapped in the `IO` monad, reinforcing Haskell's paradigm of rigid separation between pure and impure functions. 

## See Also:

- [Haskell Library: System.Random](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
- [Random Number Generation in The Haskell School of Expression](https://www.cs.yale.edu/homes/hudak/Paul/HSoE.pdf)
- [The "random-fu" Package](http://hackage.haskell.org/package/random-fu) 
- [The "mwc-random" Package](http://hackage.haskell.org/package/mwc-random)