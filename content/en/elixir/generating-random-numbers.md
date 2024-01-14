---
title:                "Elixir recipe: Generating random numbers"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a common task in many programming languages, and Elixir is no exception. Being able to generate random numbers can be useful for creating unique identifiers, generating test data, or simulating random events in a program. In this blog post, we will explore how to generate random numbers in Elixir and the various options available to do so.

## How To

To generate random numbers in Elixir, we can use the `:rand.uniform/1` function. This function takes in a range and returns a random number within that range. Let's see an example:

```Elixir
:rand.uniform(1..10)
```

This code will generate a random number between 1 and 10. We can also use the `:rand.uniform/0` function to generate a random number between 0 and 1, like this:

```Elixir
:rand.uniform()
```

We can also generate a list of random numbers using the `:rand.uniform/2` function. This function takes in a number n and a range and returns a list of n random numbers within that range. For example:

```Elixir
:rand.uniform(5, 1..100)
```

This code will generate a list of 5 random numbers between 1 and 100. Additionally, we can use the `:rand.seed/1` function to set a seed for our random number generation. This ensures that the same sequence of random numbers will be generated every time we run our code, which can be useful for testing or debugging.

## Deep Dive

Behind the scenes, Elixir uses the Erlang `:rand` module to generate random numbers. This module provides various functions for generating random numbers, including `uniform`, `uniform_s`, and `uniform_nd`. The `uniform` and `uniform_s` functions are the ones we have discussed above, while the `uniform_nd` function is used for generating multidimensional arrays of random numbers. Additionally, the `:rand` module allows us to set a seed, as we mentioned earlier, and also to specify a distribution for our random numbers, such as uniform, normal, or exponential.

## See Also

- Elixir official documentation on random number generation: https://hexdocs.pm/elixir/Random.html
- An in-depth article on the `:rand` module: https://joearms.github.io/2013/10/25/Erlang-s-random-number-generator.html