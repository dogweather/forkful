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

## Why

Generating random numbers is a fundamental skill in many programming applications. It allows for randomness and unpredictability, which is crucial in a wide range of scenarios, such as creating unique IDs, simulating data, and generating game elements.

## How To

To generate random numbers in Elixir, we can use the `:rand.uniform/1` function, which takes in a range and returns a random number within that range. Here's an example of generating a random integer between 1 to 10:

```Elixir
num = :rand.uniform(1..10)
IO.puts(num)
```
Output: 
`7` (this number will vary as it is randomly generated each time)

If we want to generate a random floating-point number, we can use the `:rand.uniform/0` function, which returns a number between 0.0 and 1.0. For example:

```Elixir
fnum = :rand.uniform()
IO.puts(fnum)
```
Output:
`0.6323350907338301`

To generate a random number within a specific range of floating-point numbers, we can use `:rand.uniform/1` with a float range instead. For instance:

```Elixir
frand = :rand.uniform(0.5..1.5)
IO.puts(frand)
```
Output:
`0.9345606871590925`

We can also generate a random boolean value using the `:rand.uniform/1` function, which returns either `true` or `false`:

```Elixir
bool = :rand.uniform([true, false])
IO.puts(bool)
```
Output:
`true` or `false` (this will randomly vary between the two options)

## Deep Dive

Elixir uses a modified version of the Mersenne-Twister algorithm, known as the "Mersenne-Twister MT19937", to generate random numbers. This algorithm is a highly efficient and reliable pseudorandom number generator (PRNG) that is widely used in many programming languages.

One thing to note is that when using the `:rand.uniform/1` function with a specific range, the upper bound is not inclusive. For instance:

```Elixir
:rand.uniform(1..3)
```
This will only return a random number between 1 and 2, as 3 is not included in the range.

If we need to generate a random number with a specific number of digits, we can use `:rand.uniform/2` with `Kernel.trunc/2` to limit the number of decimal places. For example, to generate a random 3-digit number:

```Elixir
:rand.uniform(100..999) |> Kernel.trunc(0)
```
This will return a random integer between 100 and 999 with no decimal places.

## See Also
- Official Elixir Documentation on `:rand` module: https://hexdocs.pm/elixir/Kernel.html#rand/0
- Mersenne-Twister PRNG Algorithm: https://en.wikipedia.org/wiki/Mersenne_Twister