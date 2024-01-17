---
title:                "Generating random numbers"
html_title:           "Elixir recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is a common task in computer programming. It involves generating numbers that are unpredictable and appear to be chosen by chance. Programmers use this technique for a variety of reasons, such as creating randomized passwords, simulating real-life scenarios, or selecting random elements from a list.

## How to:
To generate random numbers in Elixir, we use the `:rand` module and its `uniform/1` function. This function takes in a range and returns a random number within that range. Here's a simple example:

```
Elixir
# Generates a random number between 1 and 10
:rand.uniform(1..10) 
```
Output:
```
7
```

We can also use the `uniform/0` function to generate a decimal number between 0 and 1. Here's an example:

```
Elixir
# Generates a random decimal between 0 and 1
:rand.uniform() 
```
Output:
```
0.3845605058804908
```

We can even generate a random boolean value using `uniform/1` and a range of `true` and `false`:

```
Elixir
# Generates a random boolean value
:rand.uniform([true, false]) 
```
Output:
```
true
```

## Deep Dive:
Random number generation has a long and interesting history in the realm of computer science. In the early days, random numbers were generated using physical phenomena like radioactive decay or electronic components. With the advent of computers, pseudorandom number generators (PRNGs) were introduced, which use mathematical algorithms to create a sequence of numbers that appear to be random.

There are also alternatives to using the `:rand` module for generating random numbers in Elixir. For example, the `:crypto` module provides the `strong_rand_bytes/1` function for generating cryptographically secure random numbers. This is important when security is a concern, such as creating random encryption keys.

The `:rand` module also allows us to set a seed value, which ensures that the same sequence of random numbers is generated every time. This can be useful for testing purposes or creating reproducible results in simulations.

## See Also:
- [Elixir documentation for the :rand module](https://hexdocs.pm/elixir/Random.html)
- [Elixir documentation for the :crypto module](https://hexdocs.pm/elixir/Crypto.html)
- [Wikipedia article on random number generation](https://en.wikipedia.org/wiki/Random_number_generation)