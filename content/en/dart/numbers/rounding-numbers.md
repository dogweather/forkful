---
date: 2024-03-08 21:33:33.009749-07:00
description: "Rounding numbers is the process of adjusting a number to its nearest\
  \ whole number or to a specified number of decimal places. Programmers often round\u2026"
lastmod: '2024-03-13T22:44:59.816256-06:00'
model: gpt-4-0125-preview
summary: Rounding numbers is the process of adjusting a number to its nearest whole
  number or to a specified number of decimal places.
title: Rounding numbers
weight: 13
---

## How to:
Dart provides native methods in its core `num` type for rounding operations. Here, we'll explore methods like `round()`, `floor()`, `ceil()`, and how to round to a specific number of decimal places.

### Rounding to the nearest whole number:
```dart
var number = 3.56;
print(number.round()); // Outputs: 4
```

### Rounding down:
```dart
print(number.floor()); // Outputs: 3
```

### Rounding up:
```dart
print(number.ceil()); // Outputs: 4
```

### Rounding to a specific number of decimal places:
To round to a specific number of decimal places, we can use the `toStringAsFixed()` method, which returns a string, or use a combination of `pow` from `dart:math` for a numerical result.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // For display purposes
print(roundedString); // Outputs: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // Outputs: 3.57

// Alternatively, for a numerical result:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // Outputs: 3.57
```

While Dart's core library covers most rounding needs effectively, for more complex mathematical operations or precise rounding requirements, libraries such as `decimal` can be useful. The `decimal` library provides an easy way to work with decimal numbers without losing precision, which is especially handy for financial calculations, but for simple rounding methods as shown, the Dart core functionality is typically sufficient.
