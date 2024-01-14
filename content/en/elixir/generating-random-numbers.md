---
title:                "Elixir recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Why Generate Random Numbers in Elixir

In programming, generating random numbers can be useful for a variety of tasks such as generating test data, creating randomized elements in a game, or creating unique identifiers. In Elixir, there are built-in functions that make generating random numbers simple and efficient.

## How To

To generate a random number in Elixir, we can use the `random.uniform/2` function, which takes in a range and returns a random number within that range.

```
Elixir
Random.uniform(1..10)
# output: 7
```

We can also generate a list of random numbers using the `Enum.map/2` function and the `random.uniform/2` function. The `Enum.map/2` function allows us to apply a function to each element in a list, in this case, generating a random number for each element.

```
Elixir
Enum.map([1,2,3,4,5], fn _ -> Random.uniform(1..10) end)
# output: [6, 3, 9, 2, 8]
```

We can also use `random.seed/1` to set a specific seed for our random number generation, making it easier to reproduce results in testing or debugging.

```
Elixir
random.seed(123)
Random.uniform(1..10)
# output: 2
```

## Deep Dive

Elixir uses the Mersenne Twister algorithm for generating random numbers. This algorithm is known for its fast generation speed and high-quality randomness. It also supports parallelization, allowing for efficient generation of multiple random numbers at once.

In addition to `random.uniform/2`, Elixir also provides functions for generating random binary data and random list permutations. The `random.seed/1` function also accepts an optional `:secure` option, which uses the system's cryptographic random number generator for even more secure random number generation.

## See Also

- [Elixir Documentation for Random module](https://hexdocs.pm/elixir/Random.html)
- [Mersenne Twister algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Elixir School - Random](https://elixirschool.com/en/lessons/basics/random/)