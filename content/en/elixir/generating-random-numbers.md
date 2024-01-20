---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is the process of producing numerical values that lack any predictable patterns. Programmers use this feature for creating uniqueness, for example, generating IDs, or during game development to create unpredictable scenarios.

## How to:
To generate random numbers in Elixir, we use the Erlang built-in function `:rand.uniform/1`. This function receives a number as a parameter and generates a random number from 1 to that given number. 

Let's conveniently omit the dots when calling Erlang built-ins, courtesy of Elixir. Here's how to do it - 

```Elixir
IO.puts(:rand.uniform(10))
```

The Output can be anything between 1 to 10

```Elixir
5
```

For generating a random float, we use `uniform/0`.

```Elixir
IO.puts(:rand.uniform())
```

The Output would be a floating-point number between 0 and 1.

```Elixir
0.5672394922195978
```

## Deep Dive
Random number generation, in concept, has been around since the early days of computing, even though its implementations have changed significantly over the years. 

In the context of Elixir, `:rand.uniform/1` is being used. It employs a sophisticated, high-quality algorithm known as the exsplus algorithm, which was introduced in Erlang 19 and is a significant improvement over the earlier algorithm used. The algorithm provides better uniform distribution of values and significantly longer period length.

If you need more control over the random number generation, there are many libraries available, for instance, "Randopoxy," a library designed to enhance the capabilities of Erlang's `:rand` module.

Remember, while `:rand` functions are often sufficient, they're not perfect, considering they're pseudo-random generators, meaning, the generated output is deterministically dependent on the input. For use-cases requiring truly random numbers, like cryptography, more specialized library or algorithms are advised.

## See Also

- Elixir's official documentation: https://hexdocs.pm/elixir/main.html
- Detailed docs of the erlang `:rand` module: https://erlang.org/doc/man/rand.html
- Package Randopoxy: https://hex.pm/packages/randopoxy
- For pseudo-randomness vs true randomness, look into https://www.random.org/randomness/