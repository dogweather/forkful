---
date: 2024-03-08 21:33:24.066261-07:00
description: "How to: Dart makes it straightforward to get the length of a string\
  \ using the `length` property. Here\u2019s a basic example."
lastmod: '2024-03-13T22:44:59.812490-06:00'
model: gpt-4-0125-preview
summary: Dart makes it straightforward to get the length of a string using the `length`
  property.
title: Finding the length of a string
weight: 7
---

## How to:
Dart makes it straightforward to get the length of a string using the `length` property. Here’s a basic example:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("The length of '\(myString)' is: \(myString.length)");
  // Output: The length of 'Hello, Dart!' is: 12
}
```
This property counts the number of UTF-16 code units in the string, which corresponds to the string's length for most common use cases.

For more nuanced text processing, especially involving Unicode characters outside the Basic Multilingual Plane (BMP), consider using the `characters` package for counting grapheme clusters, which more accurately represents user-perceived characters.

First, add `characters` to your `pubspec.yaml`:

```yaml
dependencies:
  characters: ^1.2.0
```

Then, use it like so:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("The length of '\(myEmojiString)' is: \(myEmojiString.characters.length)");
  // Output: The length of '👨‍👩‍👧‍👦 family' is: 8
}
```

In this example, `myEmojiString.characters.length` gives us the length in terms of Unicode grapheme clusters, which is a more accurate representation for strings containing complex characters, like emojis or combined character marks.
