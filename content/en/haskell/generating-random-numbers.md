---
title:                "Generating random numbers"
html_title:           "Haskell recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is the process of producing a sequence of seemingly random numbers through a computer program. Programmers often use random numbers in various applications, such as games, simulations, and cryptography, to introduce an element of unpredictability and randomness.

## How to:
```Haskell
-- First, we need to import the system random module
import System.Random

-- To generate a single random number, we can use the "randomRIO" function
-- It takes two arguments: a range of numbers and a random generator
-- Let's generate a random number between 1 and 10
randomNumber <- randomRIO (1, 10) :: IO Int
-- The "randomNumber" variable now contains a randomly generated number between 1 and 10
-- Note: the "IO Int" part is needed to specify the type of the generated number

-- To generate a list of random numbers, we can use the "randomRs" function
-- It takes the same arguments as "randomRIO" but returns a list of random numbers
-- Let's generate a list of 5 random numbers between 1 and 100
randomNumbers <- randomRs (1, 100) <$> newStdGen :: [Int]
-- The "randomNumbers" variable now contains a list of 5 randomly generated numbers between 1 and 100
-- Note: the "newStdGen" part is needed to create a new random generator

-- To generate a random boolean value, we can use the "randomIO" function
-- It returns a boolean value (True or False) based on the internal state of the random generator
-- Let's generate a random boolean value
randomBoolean <- randomIO :: IO Bool
-- The "randomBoolean" variable now contains a randomly generated boolean value (either True or False)
```
Sample output:
```
randomNumber: 8
randomNumbers: [17, 58, 32, 91, 5]
randomBoolean: True
```

## Deep Dive:
Random number generators have been around since the early days of computing, with the first algorithm for generating pseudorandom numbers published in 1946 by John von Neumann. In Haskell, the "random" package provides functions for generating random numbers based on mathematical algorithms, while the "System.Random" module uses the system's internal random number generator.

There are also other ways to generate random numbers in Haskell, such as using cryptographically secure random number generators or sampling from probability distributions using the "random-fu" package.

Under the hood, random number generation involves using a seed value and an algorithm to generate a sequence of seemingly random numbers. In Haskell, the random number generator is based on the "splitmix" algorithm, which converts a seed into a stream of random numbers.

## See Also:
- [Haskell Documentation for System.Random](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Haskell Documentation for random-fu package](https://hackage.haskell.org/package/random-fu/docs/Random-Fu.html)
- [Hackage Package for cryptonite package (cryptographically secure random number generator)](https://hackage.haskell.org/package/cryptonite/docs/Crypto-Random-API.html)