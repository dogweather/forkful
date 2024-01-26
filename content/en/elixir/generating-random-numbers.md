---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:55.366372-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a way to produce unpredicted numerical values, often used in simulations, games, or as part of cryptographic operations. Programmers need randomness to test scenarios, add gameplay elements, or secure data.

## How to:

In Elixir, you can generate random numbers using functions from the `:rand` module, which is part of the Erlang standard library. Here's a quick run-through:

```elixir
# Seed the random number generator
:rand.seed(:exsplus, {1234, 5678, 91011})

# Generate a random integer
random_int = :rand.uniform(10)
IO.puts(random_int)  # This will print a random integer between 1 and 10

# Generate a random float
random_float = :rand.uniform()
IO.puts(random_float)  # This will print a random float between 0.0 and 1.0
```

Sample output might look like this (but, of course, your output will vary!):
```
7
0.44358372783200783
```

## Deep Dive

The `:rand` module hasn't always been around. In the past, Elixir used the `:random` module, but it was deprecated in favor of `:rand`. This new module provides better algorithms for random number generation. It has multiple algorithms available, but `:exsplus` is a good default for most uses.

It's crucial to seed the random number generator, otherwise it defaults to seeds based on the current time and process ID. Seeding ensures reproducibility, which is handy for tests or specific simulations.

Also, when you need cryptographically secure random numbers, you might consider alternatives like OpenSSL or other crypto libraries, since `:rand` is not suitable for cryptographic purposes due to its predictability.

## See Also

For more details on the `:rand` module, check out Erlang's official documentation: [Erlang -- rand](http://erlang.org/doc/man/rand.html)

To explore more on cryptographic secure random number generation, visit:
- [Introduction to Cryptography in Erlang/Elixir](https://blog.voltone.net/post/5)
