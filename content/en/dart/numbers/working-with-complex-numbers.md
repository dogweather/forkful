---
date: 2024-03-08 21:33:35.026149-07:00
description: "Complex numbers, consisting of a real and an imaginary part (usually\
  \ denoted as a + bi), extend the concept of the dimensionless numbers to a two-\u2026"
lastmod: '2024-03-13T22:44:59.815325-06:00'
model: gpt-4-0125-preview
summary: "Complex numbers, consisting of a real and an imaginary part (usually denoted\
  \ as a + bi), extend the concept of the dimensionless numbers to a two-\u2026"
title: Working with complex numbers
weight: 14
---

## What & Why?

Complex numbers, consisting of a real and an imaginary part (usually denoted as a + bi), extend the concept of the dimensionless numbers to a two-dimensional space. Programmers work with complex numbers in fields such as electrical engineering, quantum computing, and fluid dynamics to model phenomena that cannot be represented along a single dimension of real numbers alone.

## How to:

Dart itself does not include a built-in library for complex numbers, necessitating either the implementation of a custom complex number class or the use of a third-party library. A popular choice for scientific computing tasks, which includes support for complex numbers, is `package:scidart`.

### Implementing a Basic Complex Number Class

For simple operations, you can easily define your own complex number class:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Addition of two complex numbers
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // String representation for easy debugging
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### Using SciDart for Advanced Operations

For more complex operations or when performance is critical, the `package:scidart` offers comprehensive support for complex numbers among other scientific computing functionalities. First, add SciDart to your pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Here's how to perform basic operations with complex numbers using SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Creating complex numbers
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Addition
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Multiplication
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Sum: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Product: Complex(real: -11.0, imaginary: 41.0)
}
```

These examples demonstrate the basic manipulation and utilization of complex numbers in Dart, both through custom implementation and via the SciDart library, highlighting the flexibility and power of Dart for scientific computing tasks.
