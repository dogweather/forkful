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

## Why

Do you ever need to generate random numbers in your programming projects? Maybe you want to create a game with unpredictable events or create a simulation where each run is unique. Well, Haskell has you covered with its built-in functions for generating random numbers!

## How To

Generating random numbers in Haskell is incredibly simple. First, we need to import the `System.Random` module to access the functions we need. Then, we can use the `randomRIO` function to generate a random number within a specified range. Here's an example:

```Haskell
import System.Random

-- Generate a random number between 1 and 10
randomNum :: IO Int
randomNum = randomRIO (1,10)

-- Print the result
main :: IO ()
main = do
    num <- randomNum
    print num
    
-- Output: 7 (or any number between 1 and 10)
```

The `randomRIO` function returns an `IO Int` type, which means it performs a side-effect (generating a random number) and returns an `Int` result. So we use the `do` notation to bind the result to a variable and then print it. Simple, right?

We can also use the `randomIO` function to generate a random number within the entire range of `Int` values. And for generating random floating-point numbers, we have the `randomRIO` function, which takes in a `Float` or `Double` range.

We can even generate random values of different data types! For example, we can use the `randomR` function to generate a random `Bool` value or the `randomShuffle` function to randomly shuffle a list. The possibilities are endless!

## Deep Dive

Now that we've seen some basic examples, let's take a deeper dive into how Haskell generates random numbers. Haskell uses a pseudo-random number generator (PRNG) to produce a sequence of numbers that appear to be random. The initial state of the PRNG is determined by the current time, so each run of the program will produce a different sequence of numbers.

Haskell also has a `Random` type class which defines the functions `random` and `randomR`, allowing us to generate random values of any type that is an instance of the `Random` class. This is what allows us to generate values like `Bool` and lists, as seen in the previous examples.

Another important concept to understand is the concept of a seed. A seed is a value used to initialize the PRNG, and it determines the sequence of random numbers that will be generated. This is useful when we want to reproduce a specific sequence of random numbers. We can set a seed using the `setStdGen` function from the `System.Random` module.

## See Also

- [Haskell Documentation on Random Numbers](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [HaskellWiki: Random Numbers](https://wiki.haskell.org/Random)
- [Real World Haskell: Randomness](http://book.realworldhaskell.org/read/randomness.html)