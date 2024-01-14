---
title:    "Elixir recipe: Generating random numbers"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Why

Generating random numbers is a common task in programming, and it serves a variety of purposes. Whether you need to generate passwords, shuffle a list, or simulate random events, having the ability to generate random numbers is a useful skill to have in your programming arsenal.

# How To

To generate random numbers in Elixir, we can utilize the `:rand` module. Let's take a look at a simple example:

```Elixir
# Generate a random integer between 1 and 10
:rand.uniform(10)
```

This will output a random number between 1 and 10. But what if we want to generate a list of random numbers? We can use the `:rand` function along with the `Enum` module to achieve this:

```Elixir
# Generate a list of 5 random integers between 1 and 10
1..5 |> Enum.map(fn _ -> :rand.uniform(10) end)
```

This will output something like `[7, 5, 8, 3, 2]`.

# Deep Dive

Behind the scenes, the `:rand` module uses a PRNG (Pseudo-Random Number Generator) to generate these numbers. This means that these numbers are not truly random, but instead, they are generated using a deterministic algorithm. This allows us to replicate the same sequence of random numbers by setting a seed. For example:

```Elixir
# Set a seed and generate the same sequence of random numbers
:rand.seed(:erlang.phash2("elixir"))
:rand.uniform(10)
```

This will always output the same number (which will be different than the one generated in the previous example). We can also use this same seed to generate a list of random numbers that will always be the same:

```Elixir
# Generate a list of 5 random integers using the same seed
1..5 |> Enum.map(fn _ -> :rand.uniform(10) end)
```

This will output something like `[4, 2, 9, 5, 1]`.

# See Also

- Documentation for the `:rand` module: https://hexdocs.pm/elixir/Random.html
- How to generate random strings in Elixir: https://dev.to/joeprevite/random-string-generation-with-elixir-354
- How to generate random UUIDs in Elixir: https://medium.com/@b1nary642/how-to-generate-a-random-uuid-in-elixir-ee61c6b42cdb