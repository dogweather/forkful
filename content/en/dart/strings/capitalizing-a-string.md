---
date: 2024-03-08 21:33:48.758582-07:00
description: "How to: Dart provides simple, straightforward methods for string manipulation.\
  \ To capitalize a word or a sentence, you would typically take the first\u2026"
lastmod: '2024-03-13T22:44:59.805147-06:00'
model: gpt-4-0125-preview
summary: Dart provides simple, straightforward methods for string manipulation.
title: Capitalizing a string
weight: 2
---

## How to:


### Using Dart's Built-in Methods
Dart provides simple, straightforward methods for string manipulation. To capitalize a word or a sentence, you would typically take the first character, convert it to uppercase, and then concatenate it with the rest of the string. Here is how you could implement it:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Output: Hello world
}
```

### Capitalizing Each Word
To capitalize the first letter of each word in a string, you could split the string into words, capitalize each one, and then join them back together:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Output: Hello Dart Enthusiasts
}
```

### Using Third-party Libraries
While Dart's standard library covers basic needs, certain tasks might be more conveniently accomplished using third-party packages. A popular choice for extended string manipulation capabilities, including capitalization, is the [`recase`](https://pub.dev/packages/recase) package. After adding it to your project's `pubspec.yaml`, you can easily capitalize strings among other functionalities:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Output: Hello World
}
```

Using `recase`, you can capitalize individual words, entire sentences, or even follow other casing conventions without manually handling the string transformations.
