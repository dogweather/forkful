---
title:                "Working with complex numbers"
date:                  2024-01-25T03:00:02.219605-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers have a real part and an imaginary part (`a + bi`). They're handy in various fields like electrical engineering and quantum computing. Programmers use them to model equations that aren't solvable using only real numbers.

## How to:
Gleam lacks native complex number support. You'd usually roll your own or find a library. Here's a quick example of how you could implement basic operations:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## Deep Dive

Complex numbers were first more formally documented by Gerolamo Cardano in the 16th century. They're a natural extension of the real numbers. However, in a young language like Gleam—which prioritizes performance and type safety—such features are bare-bones (or you DIY).

In some other languages, like Python, complex numbers are built-in (`3+4j`), making life easier. In Rust or Haskell, you have libraries that offer advanced functionalities out of the box. 

Gleam's approach means you've got to handle all aspects: arithmetic, polar coordinates, exponential forms, etc. Implementing efficient, accurate operations involves careful programming, considering how floating-point behaviour can affect your results.

Remember to test thoroughly, especially edge cases! Handling complex infinity and NaN (not a number) values can trip you up if you're not careful.

## See Also
For more goodies, here's where you can dive in:
- [Gleam's Official Docs](https://gleam.run/documentation/)
- [Complex number arithmetic guide](https://tutorial.math.lamar.edu/classes/calcii/polarcomplexnumbers.aspx)
- Dig into other language's libraries for inspiration, like Rust's [num-complex](https://crates.io/crates/num-complex) or Python's [cmath module](https://docs.python.org/3/library/cmath.html).