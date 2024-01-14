---
title:                "Haskell recipe: Generating random numbers"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often need to work with random numbers for a variety of tasks, from creating unique IDs to game development. In Haskell, generating random numbers can be a useful skill to have in your arsenal. Not only does it add an element of unpredictability to your code, but it can also help with testing and debugging.

## How To

To generate random numbers in Haskell, we first need to import the `System.Random` module. This gives us access to functions and data types related to random number generation.

Let's start by generating a single random integer between 1 and 10:

```Haskell 
import System.Random

randomInt <- randomRIO (1,10)
print randomInt
```

The `randomRIO` function takes a range and returns a random number within that range. The `<-` symbol is used to assign the result of `randomRIO` to the variable `randomInt`. Finally, we use the `print` function to display the random number.

We can also generate a list of random numbers using the `randomRs` function. This function takes a range and returns a list of random numbers within that range. Let's generate a list of 5 random integers between 1 and 100:

```Haskell
import System.Random

randomInts <- take 5 . randomRs (1,100) <$> newStdGen
print randomInts
```

In this example, we first use `newStdGen` to create a new random number generator. Then, we use `randomRs` to generate an infinite list of random integers and use the `take` function to limit the list to only 5 elements. Finally, we use the `<$>` operator to apply the `take` function to the random number generator, and the `print` function to display the list.

## Deep Dive

When generating random numbers, it's important to understand the concept of pseudo-randomness. Computers can't generate truly random numbers, but they use algorithms to generate numbers that appear random. This is why we need to provide an initial "seed" to the random number generator, to ensure we get different results each time we run our code.

The `randomR` function is another way to generate random numbers in Haskell. This function takes a range and a generator, and returns a random number and a new generator. This allows us to control the state of the random number generator and ensure that we get different results each time.

```Haskell
import System.Random

(randomInt, newGen) <- randomR (1,100) <$> newStdGen
print randomInt
(randomInt, newerGen) <- randomR (1,100) <$> newGen
print randomInt
```

In this example, we use the `newStdGen` function to create a new generator, and then use the `randomR` function twice to generate two random numbers. Notice how we use the result of the first `randomR` call as the generator for the second call, ensuring that we get a different random number each time.

## See Also

- [Haskell documentation for System.Random module](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Tutorial on generating random numbers in Haskell](https://wiki.haskell.org/Random_numbers)
- [More examples of random number generation in Haskell](https://www.tutorialspoint.com/haskell/haskell_random_numbers.htm)