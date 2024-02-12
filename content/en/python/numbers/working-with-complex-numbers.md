---
title:                "Working with complex numbers"
aliases: - /en/python/working-with-complex-numbers.md
date:                  2024-01-25T03:00:01.693948-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers are a set of numbers of the form `a + bi`, where `a` and `b` are real numbers, and `i` is the imaginary unit (`i^2 = -1`). In programming, we use them to solve problems in various domains, like electrical engineering, signal processing, and quantum computing.

## How to:
Python has built-in support for complex numbers. Here's how you can play with them:

```Python
# Creating complex numbers
z = 4 + 5j
print(z)  # Output: (4+5j)

# Accessing real and imaginary parts
print(z.real)  # Output: 4.0
print(z.imag)  # Output: 5.0

# Complex arithmetic
w = 1 - 2j
print(z + w)  # Output: (5+3j)
print(z - w)  # Output: (3+7j)
print(z * w)  # Output: (14+2j)
print(z / w)  # Output: (-3.6+1.2j)

# Modulus (absolute value)
print(abs(z))  # Output: 6.4031242374328485

# Conjugate of a complex number
print(z.conjugate())  # Output: (4-5j)
```

## Deep Dive
Complex numbers were first conceptualized by Gerolamo Cardano in the 16th century. Python, among other programming languages, treats complex numbers as first-class citizens. This means they're built into the language, with easy-to-use features, avoiding the need for importing external libraries for basic operations.

However, for heavy numerical computations, Python has a library called `cmath`, which is specifically for complex numbers. It has additional functions like `exp`, `log`, and trigonometric operations. 

When Python isn't enough, you might turn to libraries like NumPy, especially for array operations involving complex numbers. NumPy provides optimized and vectorized operations that are crucial for performance in numerical computing.

## See Also
Check out these resources to learn more:

- Python's official documentation on complex numbers: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- The `cmath` module documentation: https://docs.python.org/3/library/cmath.html
- NumPy for handling arrays of complex numbers: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
