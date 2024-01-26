---
title:                "Working with complex numbers"
date:                  2024-01-25T02:59:49.632324-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers have a real part and an imaginary part (like `3 + 4i`). They're used in engineering, physics, and certain computing problems. Programmers work with them for simulations, signal processing, and solving certain types of math problems efficiently.

## How to:
Elixir doesn't have built-in complex numbers, so we roll our own or use a library, like `ComplexNum`. Here's a quick example with a lib:

```elixir
# Assuming you have ComplexNum installed
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Create complex numbers and add them
c1 = {3, 4}   # represents 3 + 4i
c2 = {2, -3}  # represents 2 - 3i
result = ComplexMath.add(c1, c2)
IO.puts "The result is: #{inspect(result)}"
```

This would output:
```
The result is: {5, 1}
```

It means the sum of `3 + 4i` and `2 - 3i` is `5 + 1i`.

## Deep Dive
Complex numbers popped up in history because regular ol' numbers couldn't handle square roots of negatives. It wasn't until the 17th century that they were taken seriously, thanks to mathematicians like René Descartes and Gerolamo Cardano. 

In Elixir, you often use tuples like `{3, 4}` for complex numbers, or use a dedicated lib to avoid reinventing the wheel. Libraries are usually better—they handle the nitty-gritty like multiplication and division, which get tricky because of the imaginary unit 'i' (FYI: `i` squared equals `-1`).

## See Also
Check out these resources:
- [ComplexNum Library](https://hex.pm/packages/complex_num) for Elixir's package manager, Hex.
- [Elixir School](https://elixirschool.com/en/), for advanced Elixir topics and exercises.
- [Erlang -- math Module](http://erlang.org/doc/man/math.html), which Elixir uses under the hood, for other mathematical needs.
