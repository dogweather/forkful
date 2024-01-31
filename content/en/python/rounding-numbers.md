---
title:                "Rounding numbers"
date:                  2024-01-25T02:59:55.559713-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means adjusting them to be closer to a simpler or more significant value. Programmers round off numbers to simplify results, limit decimal places for display, or for certain mathematical purposes.

## How to:
Here's the lowdown on rounding numbers in Python:

```python
# Round a number to the nearest integer
print(round(8.67))  # Outputs: 9

# Round a number to a specified number of decimal places
print(round(8.67, 1))  # Outputs: 8.7

# Even numbers are rounded down and odd numbers are rounded up when equidistant
print(round(2.5))  # Outputs: 2
print(round(3.5))  # Outputs: 4
```

## Deep Dive
In Python, `round()` isn't just chopping off decimals. Historically, Python, like many other languages, follows "round half to even" or "banker's rounding". This minimizes cumulative error in sums or averages, which matters in financial calculations.

For alternatives, you've got `math.floor()` and `math.ceil()` from Python's math module, dragging numbers down or up to the next whole number. But if it's precision you're after, `decimal` module's `quantize()` lets you specify rounding behavior.

Under the hood, `round()` deals with binary floating-point numbers. Since some decimals can't be expressed exactly in binary, you might get surprises with stuff like `round(2.675, 2)` not becoming `2.68` as expected. Cue in `decimal` or `fractions` for high precision.

## See Also
- Python's documentation on built-in functions: https://docs.python.org/3/library/functions.html#round
- Decimal fixed point and floating-point arithmetic: https://docs.python.org/3/library/decimal.html
- Python's math module: https://docs.python.org/3/library/math.html
