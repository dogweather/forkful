---
title:                "Rounding a number"
date:                  2024-01-24T20:57:49.865343-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding a number is the process of adjusting it to the nearest whole number or to a specified number of decimal places. Programmers round numbers to simplify values, to conform to display specifications, or to make them human-readable when exact precision isn't necessary.

## How to:

In Elixir, you have several ways to round a number. Here's a simple rundown:

```elixir
# To the nearest integer
rounded_integer = round(3.14)
IO.puts(rounded_integer)  # Output: 3

# To a specified number of decimal places, use :erlang.float_to_binary/2
rounded_float = :erlang.float_to_binary(3.14159, [decimals: 2])
IO.puts(rounded_float)    # Output: "3.14"
```

## Deep Dive

Rounding numbers is a fundamental concept in many programming languages, not just Elixir. Historically, rounding methods are deeply rooted in mathematics and have been around since ancient times, used in commerce and science to simplify calculations.

Now, the way rounding is implemented can differ. Elixir taps into the underlying Erlang VM's abilities with functions such as `round/1`, which rounds to the nearest integer. For more precise control, you can use `:erlang.float_to_binary/2`, which is an Erlang function directly available in Elixir that can convert a floating-point number to a binary (string), allowing you to specify the number of decimals.

There's always a trade-off in rounding between accuracy and simplicity. The rounding strategy used might depend on the specific use case — such as financial calculations often requiring specific types of rounding to meet compliance rules.

Alternative rounding methods include:

- `trunc/1`: Truncates the number by dropping decimals.
- `Float.round/2`: Introduced in Elixir 1.3, allows rounding to a given number of decimal places and rounding strategy.
  
In maths, there are various rounding rules like rounding half up, half down, half to even (Bankers rounding), or half away from zero, and these can be selected based on requirements.

## See Also

To get more of a handle on rounding in Elixir, check out:

- Elixir's official documentation on `Kernel.round/1`: [Elixir Kernel.round/1](https://hexdocs.pm/elixir/Kernel.html#round/1)
- Erlang's official documentation on `:erlang.float_to_binary/2`: [Erlang float_to_binary/2](https://erlang.org/doc/man/erlang.html#float_to_binary-1)
- Fundamentals of number rounding off in mathematics which is well-documented in resources like Wolfram MathWorld's Rounding entry: [MathWorld Rounding](http://mathworld.wolfram.com/Rounding.html)