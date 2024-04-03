---
date: 2024-01-27 20:26:18.067755-07:00
description: "How to: To generate random numbers in Elixir, you primarily use the\
  \ `:rand` module which provides several functions for this purpose. Here is a quick\u2026"
lastmod: '2024-03-13T22:44:59.778876-06:00'
model: gpt-4-0125-preview
summary: To generate random numbers in Elixir, you primarily use the `:rand` module
  which provides several functions for this purpose.
title: Generating random numbers
weight: 12
---

## How to:
To generate random numbers in Elixir, you primarily use the `:rand` module which provides several functions for this purpose. Here is a quick guide to get you started:

First, ensure you seed the random number generator to initialize it with a unique starting point:

```elixir
:rand.seed(:exsplus)
```

To generate a random integer within a range, use:

```elixir
random_integer = :rand.uniform(10) # Generates a number between 1 and 10
IO.puts(random_integer)
```

For a random float between 0 and 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

You might need a more specific range for floats, which requires a bit more calculation:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Remember, these numbers are pseudo-random; they're determined by the seed and algorithm but suffice for most applications.

## Deep Dive
Elixir's random number generation capabilities rely on Erlang's `:rand` module, reflecting its heritage and close relationship with Erlang. The `:rand` module replaced the older `:random` module, offering improved algorithms for random number generation. It provides a variety of algorithms, the default being `exsplus`, but also supports others like `exs64`, `exsl`, and more, each with its trade-offs in terms of speed and randomness quality.

An interesting aspect of Elixir's (and thus Erlang's) random number generation is its handling of seeds. The system maintains separate seed states for each process, ensuring that concurrent processes don't interfere with each other's random number sequences. This is particularly useful in concurrent applications, ensuring predictability and reliability in distributed systems.

While the `:rand` module suffices for most use cases, applications requiring cryptographically secure random numbers should consider other options. The `crypto` module provides functions like `crypto:strong_rand_bytes/1` that are designed to generate secure random data suitable for cryptographic purposes. These alternatives are essential for security-sensitive applications like token generation, encryption, and certain types of authentication mechanisms.
