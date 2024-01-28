---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:09.664519-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Elixir is about using built-in functions or libraries to produce numbers that appear to be the result of a random process. Programmers do this for a variety of reasons, including but not limited to simulations, gaming logic, testing, and security applications.

## How to:

Elixir provides a straightforward way to generate random numbers, primarily through the `:rand` module, which is part of the Erlang/OTP infrastructure, seamlessly accessible from Elixir. Here’s how you can start generating random numbers:

```elixir
# Seed the random number generator first
:rand.seed(:exsplus, {1234, 5678, 91011})

# Generate a random integer
random_integer = :rand.uniform(10)
IO.puts("Random Integer: #{random_integer}")

# Generate a random float between 0 and 1
random_float = :rand.uniform()
IO.puts("Random Float: #{random_float}")
```

Sample output might look something like this, though, of course, your actual output will vary:

```
Random Integer: 7
Random Float: 0.44358461764705883
```

For use cases requiring randomness that spans across different processes or nodes, you may find yourself seeding quite often or looking for ways to have a more globally accessible solution.

## Deep Dive

The `:rand` module in Elixir (and Erlang) uses different algorithms under the hood, with the default being the `exsplus` algorithm—a modified version of the Xorshift116+ algorithm, known for its high performance and good statistical properties for a wide range of applications. There was a time when Erlang used the `random` module, which was simpler but provided less statistically reliable results, leading to the adoption of `:rand` in OTP 18.0.

While the built-in capabilities of `:rand` cover many use cases, there are scenarios where more specialized random number generation might be needed, such as cryptographic applications. For these, the `:crypto.strong_rand_bytes/1` function can be utilized to generate a binary of cryptographically strong random bytes, which can then be converted into numbers as required. However, it's worth noting that this method is considerably heavier in terms of computation and should be used judiciously.

Elixir’s minimalistic approach in its core language features, choosing to leverage the robust, underlying Erlang libraries like `:rand`, exemplifies its philosophy of being a small, extensible language. This strategy allows Elixir programs to achieve broad functionality without bloating the language with specialized functions for every possible use case.

## See also

### Official Elixir Documentation
- [Erlang `:rand` Module](https://erlang.org/doc/man/rand.html)

### Tutorials and Guides
- **Elixir School**: [Random Numbers](https://elixirschool.com/en/lessons/basics/randomness/)
- **Medium**: [Random Number Generation in Elixir](https://medium.com/@feymartynov/random-number-generation-in-elixir-b7caf04a9552)

### Blog Posts and Articles
- **Poeticoding**: [Dealing with Randomness in Elixir](https://poeticoding.com/dealing-with-randomness-in-elixir/)
- **Dev.to**: [Random Number in Elixir](https://dev.to/fazibear/random-number-in-elixir-1n4)
