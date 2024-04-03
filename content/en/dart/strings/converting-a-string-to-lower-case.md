---
date: 2024-03-08 21:33:50.597368-07:00
description: "How to: In Dart, you can convert a string to lowercase using the `toLowerCase()`\
  \ method provided by the `String` class. This method returns a new string\u2026"
lastmod: '2024-03-13T22:44:59.808746-06:00'
model: gpt-4-0125-preview
summary: In Dart, you can convert a string to lowercase using the `toLowerCase()`
  method provided by the `String` class.
title: Converting a string to lower case
weight: 4
---

## How to:
In Dart, you can convert a string to lowercase using the `toLowerCase()` method provided by the `String` class. This method returns a new string with all uppercase characters converted to lowercase. Let's see how this works with a simple example:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Output: hello, world!
}
```

Dart does not require external libraries for basic string manipulation tasks, including converting to lowercase, as the standard library's `String` class is quite comprehensive. However, for more complex manipulations involving locale-specific rules, you might consider the `intl` package, which provides internationalization and localization facilities, including case conversion based on locale:

To use `intl`, add it to your `pubspec.yaml` file:

```yaml
dependencies:
  intl: ^0.17.0
```

Then, you can use the `toLocaleLowerCase()` method to convert a string to lowercase based on specific locales:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Turkish Locale
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Output: istanbul
  
  // Default Locale (en)
  print(originalString.toLowerCase()); // Output: i̇stanbul
}
```

In this example, notice how the Turkish locale correctly handles the dotless 'i', showcasing the importance of locale-aware transformations in internationalized applications.
