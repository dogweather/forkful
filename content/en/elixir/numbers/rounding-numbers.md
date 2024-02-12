---
title:                "Rounding numbers"
aliases:
- en/elixir/rounding-numbers.md
date:                  2024-01-25T02:59:36.408769-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means adjusting them to a nearby value for simplicity or to match a certain precision. It's useful for improving readability, reducing storage space, or meeting domain-specific needs, like money calculations where you want to round to the nearest cent.

## How to:
In Elixir, you can use `Float.round/2` to round a floating-point number. You can specify the number of decimal digits you want to keep. Here's how it works:

```elixir
# Round a number to no decimal places
Float.round(3.14159) # => 3.0

# Round a number to 2 decimal places
Float.round(3.14159, 2) # => 3.14

# Round a number to a negative precision to the nearest 10
Float.round(123.456, -1) # => 120.0
```

## Deep Dive
Rounding numbers is a classic problem in computer science—so much so that the choice of rounding strategy can impact financial systems, scientific calculations, and more. Elixir's `Float.round/2` defaults to "half up" rounding, resembling traditional rounding taught in math class.

If you need other types of rounding, Elixir lets you roll your own. Consider, for example, "floor" rounding (always down) or "ceiling" rounding (always up). You'd use `Float.floor/1` or `Float.ceil/1`, respectively.

```elixir
# Floor rounding
Float.floor(3.999) # => 3.0

# Ceiling rounding
Float.ceil(3.001) # => 4.0
```

These alternatives help tailor rounding to the exact needs of your application, whether it's financial calculations, graphics rendering or data approximation.

## See Also
For more on Elixir's rounding functions and floating-point numbers:

- Elixir's official docs on `Float`: https://hexdocs.pm/elixir/Float.html
- IEEE Standard for Floating-Point Arithmetic (IEEE 754): https://ieeexplore.ieee.org/document/4610935
