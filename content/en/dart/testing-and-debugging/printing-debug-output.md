---
date: 2024-03-08 21:33:33.906560-07:00
description: 'How to: In Dart, you can print debug output using the `print()` function.
  Here''s how to output simple messages and variable values.'
lastmod: '2024-03-13T22:44:59.823116-06:00'
model: gpt-4-0125-preview
summary: In Dart, you can print debug output using the `print()` function.
title: Printing debug output
weight: 33
---

## How to:
In Dart, you can print debug output using the `print()` function. Here's how to output simple messages and variable values:

```dart
void main() {
  String greeting = "Hello, Dart!";
  print(greeting); // Prints: Hello, Dart!

  int number = 42;
  print('The number is $number.'); // Prints: The number is 42.
}
```

For structured data, like lists or objects, Dart's `toString()` method may not provide enough detail. In those cases, you can use the `jsonEncode` function from Dart's `dart:convert` library to convert the data to a JSON string for more readable output:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // Prints: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

When more sophisticated debugging capabilities are needed, such as logging with different levels of importance (info, warning, error), you can use third-party libraries like `logger`. Here's how to use it:

1. Add `logger` to your `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Use `logger` in your Dart code:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("This is a debug message");
  logger.w("This is a warning message");
  logger.e("This is an error message");
}
```

The output will be more informative, showing the level of the message and the message itself, making it easier to distinguish between different kinds of log messages.
