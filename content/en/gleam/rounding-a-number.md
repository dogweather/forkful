---
title:                "Rounding a number"
date:                  2024-01-24T20:57:21.022361-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding a number is the process of adjusting it to its nearest whole number or to a specified degree of decimal precision. Programmers round numbers to simplify them for human understanding, to save space, or to comply with mathematical rules or business logic.

## How to:

In GLEAM, rounding a number is straightforward. Below are examples showing how to round a number to the nearest whole number and to two decimal places.

```ProgLang.GLEAM
import gleam/float

// Round to the nearest whole number
let num = 3.14159
let rounded = float.round(num)
// Sample output: 3.0

// Round to two decimal places
let precision = 2
let rounded_two = float.round_to(num, precision)
// Sample output: 3.14
```

## Deep Dive

Historically, rounding numbers is a concept that stems from various mathematical practices and needs. There are different methods of rounding: Round up (ceil), round down (floor), and round to the nearest (the common one discussed here).

In programming, different languages have different built-in functions for rounding, and some even provide multiple methods to handle different rounding rules, like bankers' rounding or arithmetic rounding.

The implementation of rounding in GLEAM is part of its float module. It uses the IEEE-754 standard for floating-point arithmetic, ensuring consistent behavior across different systems. Rounding can introduce errors, especially when dealing with floating-point numbers due to their binary representation in computers.

## See Also

- GLEAM documentation on Floats: [Official Gleam float documentation](https://gleam.run/stdlib/float/)
- Wikipedia on Rounding: [Rounding - Wikipedia](https://en.wikipedia.org/wiki/Rounding)
- IEEE Standard for Floating-Point Arithmetic (IEEE 754): [IEEE 754-2008](https://ieeexplore.ieee.org/document/4610935)