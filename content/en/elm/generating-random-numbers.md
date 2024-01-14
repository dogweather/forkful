---
title:                "Elm recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Why: Exploring the Fun and Functionality of Random Numbers in Elm

Have you ever wanted to add some unpredictability to your Elm programs? Look no further than the power of random numbers! Not only can they add a touch of excitement to your code, but they also have practical applications in games, simulations, and much more.

# How To: Generating Random Numbers in Elm

To get started with random numbers in Elm, we first need to import the Random module. Then, we can use the `generate` function to create a random number generator. Here's an example of generating a random integer between 1 and 10:

```Elm
import Random exposing (..)

randomNumGenerator : Random.Generator Int
randomNumGenerator =
  generate (Int.range 1 10)

randomNum : Int
randomNum =
  Random.generate randomNumGenerator
```

In this code, we first define a generator that uses the `Int` range function to specify a range of numbers. Then, we use the `generate` function to actually create a random number based on that generator. Now, every time the `randomNum` variable is called, a different integer between 1 and 10 will be generated.

We can also use the `generate` function to create random numbers of different types, including floats, booleans, and even lists. Check out the [official Elm documentation](https://package.elm-lang.org/packages/elm/random/latest/Random#generate) for more examples and details.

# Deep Dive: Understanding Randomness in Elm

The `generate` function in Elm uses a seed to generate pseudo-random numbers. This means that the same seed will always produce the same sequence of random numbers. However, by changing the seed, we can get a different sequence of numbers.

Why is this important? Well, it allows us to create reproducible results in our programs. This is especially useful in testing and debugging scenarios. We can also use different seeds to create different "playthroughs" of our programs, adding even more variety to our applications.

# See Also

For more information on random numbers in Elm, check out these helpful links:

- [Official Elm Random Module Documentation](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [Article: Using Random Numbers in Elm](https://thoughtbot.com/blog/generating-random-values-in-elm)
- [Tutorial: Understanding Randomness in Elm](https://becoming-functional.com/understanding-randomness-in-elm-7d2f51c6c416)