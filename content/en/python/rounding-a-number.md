---
title:                "Rounding a number"
date:                  2024-01-24T20:57:42.977598-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding a number essentially means adjusting it to the nearest whole number or to a specified number of decimal places. Programmers do this to simplify figures, making them more readable, or to meet certain mathematical constraints in their computations.

## How to:

In Python, you have a couple of tools at your disposal for rounding numbers; the built-in `round()` function is the go-to:

```python
number = 7.765
rounded_number = round(number)  # Rounds to nearest whole number
print(rounded_number)  # Output: 8
```

You can also specify the number of decimal places you want:

```python
rounded_number = round(number, 1)  # Rounds to one decimal place
print(rounded_number)  # Output: 7.8
```

Another way to round is to use floor and ceiling functions from the `math` module:

```python
import math

number = 7.765

# Always rounds down
print(math.floor(number))  # Output: 7

# Always rounds up
print(math.ceil(number))  # Output: 8
```

## Deep Dive

Long before Python, the concept of rounding has existed in mathematics. Its inclusion in programming languages was a natural progression, as calculations often require numbers to be approximated to match the precision of the real world (like currency calculations).

For the alternatives, Python doesn't limit you to its built-in `round()` function. Libraries like `math`, `numpy`, and `decimal` offer their own rounding functions, each with their nuances. For example, `decimal.Decimal` provides a context where you can specify rounding behavior. This is crucial in financial applications where rounding can have a significant effect on outcomes.

The implementation detail that catches most new Python users off-guard is how `round()` handles midway cases:

```python
print(round(2.5))  # Output: 2
print(round(3.5))  # Output: 4
```

Python's `round()` uses a strategy called "round half to even", also known as "bankers' rounding", which minimizes cumulative error in repeated calculations.

## See Also

- Check the [Python documentation on built-in functions](https://docs.python.org/3/library/functions.html#round), especially `round()`.
- [decimal — Decimal fixed point and floating-point arithmetic](https://docs.python.org/3/library/decimal.html) in the Python Standard Library.
- For extensive numerical computations, delve into [NumPy's rounding capabilities](https://numpy.org/doc/stable/reference/routines.math.html#rounding).