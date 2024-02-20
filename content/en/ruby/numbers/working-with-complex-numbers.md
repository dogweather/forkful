---
date: 2024-01-25 02:59:41.349298-07:00
description: "Complex numbers, comprised of a real and an imaginary part (like 3+4i),\
  \ are a staple in engineering and physics. Programmers work with them in\u2026"
lastmod: 2024-02-19 22:05:19.005289
model: gpt-4-1106-preview
summary: "Complex numbers, comprised of a real and an imaginary part (like 3+4i),\
  \ are a staple in engineering and physics. Programmers work with them in\u2026"
title: Working with complex numbers
---

{{< edit_this_page >}}

## What & Why?
Complex numbers, comprised of a real and an imaginary part (like 3+4i), are a staple in engineering and physics. Programmers work with them in simulations, signal processing, and solving equations that don't play nice with just real numbers.

## How to:
Ruby makes handling complex numbers a breeze. You can create and manipulate them using the Complex class:

```ruby
require 'complex'

# Create complex numbers
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Basic operations
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Conjugate, magnitude, and phase
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 radians

# Complex-specific methods
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## Deep Dive
Complex numbers aren't new—they've been around since the 16th century, solving equations with no real solutions. Math aside, computationally, Ruby's Complex class does the heavy lifting, backed by the Math module for trigonometric and transcendental functions.

Earlier programming languages required manual handling of real and imaginary parts. Some, like Fortran and C++, dedicate special libraries to complex arithmetic.

Ruby's approach embeds complex number support in its syntax, freeing you from reinventing the wheel. Behind the scenes, the Complex class handles the math, while Ruby takes care of object interactions.

## See Also
- Ruby Docs on Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorld's take on Complex Numbers: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- A visual introduction to complex numbers and why they're useful: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
