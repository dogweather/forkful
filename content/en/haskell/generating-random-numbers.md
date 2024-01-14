---
title:                "Haskell recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Why Generate Random Numbers?

Generating random numbers is a fun and useful programming exercise that can also have practical applications in various algorithms and simulations. In Haskell, it is also a great way to showcase the power of functional programming and its ability to work with pure functions.

# How To Do It: A Simple Example

To generate a random number in Haskell, we can use the *random* function from the *System.Random* module. First, we need to import the module:

```Haskell
import System.Random
```

Next, we can create a function called *randomNumber* that will take in a range and use the *randomRIO* function to generate a random number within that range:

```Haskell
randomNumber :: (Int, Int) -> IO Int
randomNumber range = randomRIO range
```

Here, we are using the *IO* monad to wrap the result in the *IO Int* type, meaning that we will get an *IO* computation that will eventually yield an *Int* value. Now, let's call the function and see the result:

```Haskell
result <- randomNumber (1, 10)
```

The *result* will be a random number between 1 and 10. Run the code multiple times and you will get different results each time!

# Deep Dive into Generating Random Numbers

The *random* function uses a pseudo-random algorithm to generate numbers. This means that the numbers are not truly random, but they appear to be random for practical purposes.

The *random* function has a type of *RandomGen g => g -> (a, g)* where *a* is the type of the result and *g* is the type of the random number generator. This allows the function to work with different types of random number generators, such as *StdGen* or *IO StdGen*.

If we want to get a specific sequence of random numbers, we can use the *mkStdGen* function to create a specific random number generator with a given seed:

```Haskell
gen <- mkStdGen 42
```

Now, every time we use *gen* to generate a random number, we will get the same sequence of numbers. This can be useful for testing or reproducibility.

# See Also

- [Haskell Random Module Documentation](https://hackage.haskell.org/package/random)
- [Functional Random Numbers in Haskell](https://codecrafters.io/posts/functional-random-numbers-in-haskell)
- [Haskell Tutorial: Random Numbers and Generating Games](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10-randomness)