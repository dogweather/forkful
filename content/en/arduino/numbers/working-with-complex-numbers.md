---
date: 2024-01-25 02:59:36.547679-07:00
description: "How to: Originally, complex numbers were met with skepticism, but they've\
  \ become central in various scientific fields. Historically, they were recognized\u2026"
lastmod: '2024-04-05T22:50:48.872422-06:00'
model: gpt-4-1106-preview
summary: Originally, complex numbers were met with skepticism, but they've become
  central in various scientific fields.
title: Working with complex numbers
weight: 14
---

## How to:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Start serial communication
  
  Complex myComplex(2, 3); // Create a complex number 2 + 3i
  Complex anotherComplex(1, 1); // Create another complex number 1 + 1i
  
  // Addition
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addition: "); 
  result.print(); // Outputs 3 + 4i
  
  // Multiplication
  result = myComplex * anotherComplex; 
  Serial.print("Multiplication: ");
  result.print(); // Outputs -1 + 5i
}

void loop() {
  // Not used in this example
}
```
Sample output:
```
Addition: 3 + 4i
Multiplication: -1 + 5i
```

## Deep Dive
Originally, complex numbers were met with skepticism, but they've become central in various scientific fields. Historically, they were recognized for providing solutions to polynomial equations that lack real solutions. 

Arduino doesn't include complex numbers in its standard library, but you can leverage libraries like `Complex.h` for handling them. Internally, these libraries define a Complex class, typically using two doubles to store the real and imaginary parts, and overload operators to support arithmetic.

As an alternative, for applications that don't inherently need complex number arithmetic, consider using other math strategies or libraries. Remember, though, that using floats instead of complex numbers could oversimplify some problems.

## See Also
- The [Complex.h](https://github.com/RobTillaart/Complex) library by Rob Tillaart.
- A deeper dive into the [math behind complex numbers](https://mathworld.wolfram.com/ComplexNumber.html).
