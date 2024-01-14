---
title:    "Elm recipe: Generating random numbers"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why
As programmers, we often need to use random numbers in our applications. Whether it's for creating game mechanics or generating unique IDs, random numbers can add an element of unpredictability and complexity to our code. Elm offers a simple and elegant way to generate random numbers, making it a popular choice among developers.

## How To
Generating random numbers in Elm is easy and straightforward. The `Random` library comes built-in with the language, so there's no need to install any external packages. To begin, we'll create a simple program that generates a random number between 1 and 100.

```
module Main exposing (main)

import Html exposing (text)
import Random exposing (..)

main = 
  text (String.fromInt (generateRandomNum 1 100))

generateRandomNum min max =
  Random.int min max
```

In this code, we first import the `Html` and `Random` libraries. Then, in the `main` function, we call the `generateRandomNum` function, which takes in the range of numbers we want our random number to fall within. Finally, we use `text` and `String.fromInt` to display the generated random number on our webpage.

To generate a list of random numbers, we can use the `list` function from the `Random` library. For example, if we want to create a list of 10 random numbers between 1 and 100, we can modify our code as follows:

```
main = 
  text (String.fromList (generateRandomList 10 1 100))

generateRandomList num min max =
  Random.List.generate num (Random.int min max)
```

This `generateRandomList` function takes in the number of random numbers we want, along with the range, and uses the `Random.List.generate` function to create a list of random numbers.

## Deep Dive
Under the hood, Elm uses a `Random.Generator` to create random numbers. This generator works by taking in a seed value and producing a new value based on this seed. Each time we call a random number function, we are essentially creating a new generator with a new seed.

Additionally, we can use the `Random.step` function to manually manipulate the generator and control the randomness of the numbers being generated. This can be useful for creating reproducible results or creating specific distribution patterns.

## See Also
- [Elm Random library documentation](https://package.elm-lang.org/packages/elm/random/latest/)
- [Generating random numbers in Elm blog post](https://dev.to/sophiabrandt/generating-random-numbers-in-elm-42p9)

With the simple and efficient random number generation capabilities of Elm, we can add an extra layer of complexity and fun to our applications. So go ahead, try it out and see what kind of random numbers you can generate!