---
title:                "Working with complex numbers"
aliases:
- /en/c/working-with-complex-numbers.md
date:                  2024-02-03T17:50:22.789970-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with complex numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Complex numbers consist of a real part and an imaginary part, represented as `a + bi` where `i` is the square root of `-1`. Programmers work with complex numbers in various fields such as electrical engineering, quantum computing, and fluid dynamics, harnessing their unique properties for simulations, signal processing, and solving specific types of mathematical equations.

## How to:

In C, complex numbers are supported by the Standard Library, specifically `<complex.h>`. To utilize them, declare variables with the `double complex` type (or `float complex` for single precision). Here's how to perform basic operations:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Declare a complex number 1+2i
    double complex z2 = 1.0 - 2.0*I; // Declare another complex number 1-2i
    
    // Addition
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // Output: Sum: 2.00 + 0.00i

    // Multiplication
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // Output: Product: 5.00 + 0.00i

    // Complex Conjugate
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // Output: Conjugate of z1: 1.00 - 2.00i
    
    // Magnitude
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // Output: Magnitude of z1: 2.24

    // Phase
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // Output in radians
    
    return 0;
}
```
Note that `I` is a constant representing the imaginary unit in `<complex.h>`. Functions like `creal()` and `cimag()` extract real and imaginary parts, respectively, while `conj()` computes the complex conjugate. For the magnitude and phase (argument) of complex numbers, `cabs()` and `carg()` are used.

## Deep Dive

The support for complex numbers in C is relatively recent, having been standardized in C99. Prior to this, complex number arithmetic in C was cumbersome, often requiring custom data structures and functions. The inclusion of `<complex.h>` and the complex data types provided a significant boost to the language's capabilities for scientific and engineering applications. However, it's worth noting that some languages, like Python, offer more intuitive support for complex numbers through built-in data types and a richer set of library functions. Despite this, the performance and control offered by C make it a preferred choice for high-performance computing tasks, even if it means dealing with a slightly more verbose syntax for complex arithmetic.
