---
title:    "Haskell recipe: Generating random numbers"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Haskell is a popular functional programming language known for its strong static typing and the ability to handle complex mathematical operations with ease. One of the most interesting features of Haskell is the ability to generate random numbers. In this blog post, we will discuss why generating random numbers can be useful and how to do it in Haskell.

## How To

Generating random numbers in Haskell is a fun and simple task. To do so, we will be using the `random` package, which comes pre-installed with most Haskell installations.

First, we need to import the `random` module in our code. Next, we can use the `randomRIO` function to generate a random number within a specified range. Here is a sample code:

```Haskell
import System.Random

main = do
    number <- randomRIO (1, 10)
    putStrLn $ "Your random number is: " ++ show number
```

This code will generate a random number between 1 and 10 and print it to the console. Running this code multiple times will result in different random numbers being generated.

We can also generate a list of random numbers by using the `randomRs` function. This function takes in a range and a seed as parameters. Here is an example:

```Haskell
import System.Random

main = do
    let list = take 5 $ randomRs (1, 10) (mkStdGen 7)
    putStrLn $ "Your list of random numbers is: " ++ show list
```

This code will generate a list of 5 random numbers between 1 and 10, using the seed `7`. Using the same seed will result in the same list of numbers being generated, making it useful for testing and debugging purposes.

## Deep Dive

Behind the scenes, Haskell uses a Random Number Generator (RNG) to generate random numbers. The `randomRIO` and `randomRs` functions use the system's default RNG, which is usually a deterministic generator based on the system time. However, we can also use a custom RNG by using the `mkStdGen` function. This allows for more control over the randomness and makes it easier to reproduce results.

There are also other functions available in the `random` package for generating different types of random numbers, such as `randomIO` for generating any random value, not just numbers, and `randomShuffle` for shuffling a list.

## See Also

To learn more about generating random numbers in Haskell, check out the following resources:

- [Haskell Random package documentation](https://hackage.haskell.org/package/random)
- [Real World Haskell - Randomness](http://book.realworldhaskell.org/read/randomness.html)