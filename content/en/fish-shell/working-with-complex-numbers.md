---
title:                "Working with complex numbers"
date:                  2024-01-25T02:59:58.849195-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers extend the idea of one-dimensional number lines to a two-dimensional complex plane. Programmers use them in fields like engineering, physics, and graphics for calculations that require two components, like signals or rotations.

## How to:
In Fish, we handle complex numbers using `math` with real and imaginary parts. Here’s a kick-off:

```fish
# Add two complex numbers (3+4i) and (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Outputs: 8+6i

# Multiply two complex numbers (1+2i) and (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Outputs: -5+10i
```

If you need to raise a complex number to a power or get its exponential form:

```fish
# Square of (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Outputs: -5+12i

# Exponential of (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Outputs: -0.41615+0.9093i
```

## Deep Dive
Fish Shell's math support for complex numbers is relatively new, kicking off around version 3.1.0. Before that, people might have used `bc` or called out to external tools like Python for complex math.

Alternatives to Fish's math include specialized numerical libraries or languages like MATLAB, Python with NumPy, or even C++ with the Standard Library. However, these might be overkill for quick shell calculations.

Fish's complex number support is baked into its internal `math` command, leveraging libcalc. This means you don't have to install extra tools for basic operations.

However, Fish isn't designed for heavy mathematical computation. Its maths ability is convenient for quick calculations or scripts where complex numbers come into play, but consider more robust tools for intensive tasks.

## See Also
- Fish shell documentation for math: https://fishshell.com/docs/current/commands.html#math
- NumPy for Python, a popular alternative: https://numpy.org/
- A deeper look into complex numbers: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/