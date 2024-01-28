---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:03.658752-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Haskell involves creating values that cannot be predetermined by the user, typically for applications in simulations, games, and testing scenarios. Programmers utilize randomness to mimic the unpredictability of real-world events or to generate test data that covers a wide range of possible inputs.

## How to:

Haskell provides a rich library, `System.Random`, for generating random numbers. The library allows the generation of random values in a pure functional way, using the `Random` typeclass. Let's dive into a basic example to generate a random integer within a range.

First, ensure you have the `random` package installed. If not, you can get it via Cabal or Stack.

Here is a straightforward way to generate a random number between 1 and 100:

```haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 100) :: IO Int
  putStrLn $ "Your random number: " ++ show randomNumber
```

Sample output might be:

```
Your random number: 42
```

For more controlled scenarios where you need reproducible sequences of random numbers, you can utilize the `StdGen` generator:

```haskell
import System.Random (newStdGen, randomRs)

main :: IO ()
main = do
  gen <- newStdGen
  let randomNumbers = take 5 $ randomRs (1, 100) gen :: [Int]
  print randomNumbers
```

This will output a list of 5 random numbers within the specified range, for instance:

```
[28, 76, 45, 35, 17]
```

## Deep Dive

The core of Haskell's random number generation lies in its ability to maintain purity while dealing with inherently impure operations like generating random numbers. This is achieved through the `IO` monad for impure functions and the use of pure functions with explicit seeds for reproducibility.

Historically, the Haskell approach to randomness has evolved. Early versions required more boilerplate and explicit passing of the random generator. The `System.Random` library simplifies these processes, but discussions in the Haskell community have highlighted areas for improvement, such as performance and the API's simplicity, leading to alternative libraries such as `mwc-random` for performance-critical applications.

The design of the random number generation in Haskell, especially with `System.Random`, offers a balance between the purity required by functional programming and the need for practical application usage. However, when high performance or more control over random number generation is required, looking into alternatives or newer proposals in the Haskell ecosystem might be worthwhile.

## See also

### Official Haskell Documentation
- [Haskell `random` Package](https://hackage.haskell.org/package/random)

### Tutorials and Guides
- **School of Haskell**: [Random Number Generation](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)
- **Haskell Wiki**: [Haskell in 5 steps - Random numbers](https://wiki.haskell.org/Haskell_in_5_steps#Random_numbers)

### Example Projects
- **GitHub**: [Haskell Random Number Examples](https://github.com/search?q=haskell+random+numbers&type=Code)
