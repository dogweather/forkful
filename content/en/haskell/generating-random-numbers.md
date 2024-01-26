---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:07.872387-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is like rolling dice with your code; you get unpredictability. Programmers need randomness for simulations, games, and sometimes for shuffling data or security purposes.

## How to:

First off, let’s get our imports in:

```Haskell
import System.Random (randomRIO)
```

Now, here’s how to roll a six-sided dice:

```Haskell
main :: IO ()
main = do
  result <- randomRIO (1, 6)
  print result
```

Running this is like throwing a dice:

```
4
```

Each run potentially gives a different number, 1 through 6.

Want a random letter? Let's pick from 'a' to 'z':

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  char <- randomRIO ('a', 'z')
  print char
```

Sample output could be:

```
'e'
```

Generate a list of randoms? Use `replicateM`:

```Haskell
import System.Random (randomRIO)
import Control.Monad (replicateM)

randomList :: Int -> (Int, Int) -> IO [Int]
randomList n bounds = replicateM n (randomRIO bounds)

main :: IO ()
main = do
  numbers <- randomList 5 (1,100)
  print numbers
```

This might output something like:

```
[42, 7, 95, 13, 76]
```

## Deep Dive

Haskell, a pure functional language, handles randomness through the `IO` monad to maintain purity. A random number generator (RNG) typically uses an algorithm, providing a sequence of numbers that only seem random (pseudo-random).

Historically, RNGs often started with a 'seed' value. The new Haskell approach, embodied in the `randomRIO` function, abstracts seed management so you don't need to pass it around.

Alternatives include the `random` package for more control. It lets you choose RNGs and manage seeds explicitly if needed.

Implementing custom algorithms in Haskell is straightforward but remember, Haskell values purity and thread safety, so make sure your custom solution respects that paradigm.

## See Also

1. [Hackage: random library](https://hackage.haskell.org/package/random)
3. [School of Haskell: Basics of Random in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)
