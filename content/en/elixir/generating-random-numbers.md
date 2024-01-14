---
title:    "Elixir recipe: Generating random numbers"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

Random numbers are an essential part of many programming tasks, from generating unique identifiers to simulating games or statistical processes. In Elixir, there are multiple ways to generate random numbers, making it a versatile and useful language for these types of tasks.

## How To

Generating random numbers in Elixir is a straightforward process. To generate a single random number, we can use the `:rand.uniform/0` function, which generates a float between 0.0 and 1.0.

```elixir
:rand.uniform() # output: 0.541512
```

To generate a random integer within a specific range, we can use the `:rand.uniform/1` function, passing in the desired range as a tuple.

```elixir
:rand.uniform({1, 10}) # output: 8
```

If we want to generate multiple random numbers, we can use the `Stream` module and the `:rand.uniform/0` function to generate an infinite stream of random numbers. We can then use the `Enum.take/2` function to take a specified number of random numbers from the stream.

```elixir
Stream.repeatedly(fn -> :rand.uniform() end) |> Enum.take(5) 
# output: [0.809688, 0.211992, 0.63823, 0.836724, 0.356106]
```

We can also generate random numbers from a custom distribution using the `:rand.uniform/1` function, passing in a distribution function as an argument. For example, to generate random numbers from a normal distribution with a mean of 0 and standard deviation of 1, we can use `:rand.normal/0`.

```elixir
:rand.normal() # output: 0.89202
:rand.normal() # output: -1.56723
```

## Deep Dive

Under the hood, Elixir uses the `:random` module to generate random numbers. The `:random` module implements the Mersenne Twister algorithm, which is a high-quality pseudorandom number generator. Elixir also provides a `:rand` module, which is built on top of `:random` and provides a more convenient interface.

It is worth noting that despite being pseudorandom, the output of Elixir's random number generator is deterministic. This means that given the same seed, the same sequence of random numbers will be generated. We can use the `:rand.seed/0` function to reset the seed of the random number generator, ensuring a different sequence of numbers is generated each time.

## See Also

- [Elixir `:random` module documentation](https://hexdocs.pm/elixir/Random.html)
- [Elixir `:rand` module documentation](https://hexdocs.pm/elixir/Kernel.Rand.html)