---
title:    "Haskell recipe: Generating random numbers"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Welcome to our blog on generating random numbers in Haskell! As a functional programming language, Haskell has a powerful built-in feature for generating random numbers. This can be incredibly useful for a variety of applications, from creating computer games to running simulations.

## How To
To demonstrate how to generate random numbers in Haskell, let's start with a simple code example. First, we need to import the `System.Random` module, which is responsible for generating random numbers.

```Haskell
import System.Random

-- Generate a random integer between 1 and 10
randomInt :: IO Int
randomInt = randomRIO (1,10)

main = do
    num <- randomInt
    putStrLn $ "Random number: " ++ show num
```

When we run this code, we get a different random number between 1 and 10 every time. For example, the output might be "Random number: 8" or "Random number: 3". This is because Haskell's `randomRIO` function generates numbers based on a global random number generator, ensuring that each number is truly random.

Now, let's try generating a random boolean value using `randomIO`, which generates either `True` or `False`.

```Haskell
-- Generate a random boolean value
randomBool :: IO Bool
randomBool = randomIO :: IO Bool

main = do
    value <- randomBool
    putStrLn $ "Random boolean value: " ++ show value
```

The output of this code could be either "Random boolean value: True" or "Random boolean value: False". We can also generate random values of other data types, such as characters and floating point numbers, by using the `randomR` and `randomIO` functions.

## Deep Dive
Now that we have some basic understanding of how to generate random numbers in Haskell, let's explore some deeper aspects of this topic. Haskell's random number generation relies on a concept called "monads", which allows for maintaining a pure functional programming style while still generating random values. Without monads, generating random numbers in Haskell would not be possible.

Additionally, Haskell provides a way to generate reproducible random numbers by using the `mkStdGen` function. This allows us to specify a seed value, which will generate the same sequence of random numbers every time we run the code. This can be useful for testing and debugging purposes.

## See Also
To learn more about generating random numbers in Haskell, check out these helpful resources:

- [Haskell Wiki: Random Number Generation](https://wiki.haskell.org/Random_number_generation)
- [Real World Haskell: I/O and Randomness](http://book.realworldhaskell.org/read/io.html)
- [Haskell For All: Generating Random Numbers in Haskell](https://www.haskellforall.com/2016/05/random-number-generation-in-haskell.html)

Happy coding!