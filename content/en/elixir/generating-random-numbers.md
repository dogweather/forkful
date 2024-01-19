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

Generating random numbers is the process of producing a sequence that doesn't follow a discernable pattern. Programmers use random numbers for various purposes like creating unique IDs, in game development, and implementing encryption algorithms.

## How to:

Elixir provides a `rand/0` function to generate random numbers. Here's how you can generate a random number between 0 and 1 using it:

```Elixir
random_number = :rand.uniform()
IO.inspect(random_number)
```
You might see something like this as output:
```
0.5764609727982293
```
If you want a random integer within a specific range, say 1 and 10, you'd use the `uniform/1` function:

```Elixir
random_integer = :rand.uniform(10)
IO.inspect(random_integer)
```

Sample output could be:

```
7
```

## Deep Dive

Historically, generating random numbers was not truly random in many languages. There were algorithms like linear congruential generators, used due to their efficiency but not offering true randomness.

Elixir, by relying on the Erlang/OTP, uses an improved random number generation algorithm known as `exsplus` which provides better randomness.

An alternative to `:rand.uniform/0,1` would be using external libraries like `:crypto.strong_rand_bytes/1` - often used when you need stronger randomness for cryptography.

Always keep in mind that the `:rand` functions use a pseudo-random number generator, and it has a state that persists for each process. This means that the random numbers generated in different processes will not affect each other.

## See Also:

1. Elixir's official documentation on `:rand` module: [https://erlang.org/doc/man/rand.html](https://erlang.org/doc/man/rand.html)
2. More on random number generation history: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
3. External library for randomness: [https://hexdocs.pm/elixir/Crypto.html](https://hexdocs.pm/elixir/Crypto.html)