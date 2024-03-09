---
title:                "Searching and replacing text"
date:                  2024-03-08T21:33:32.977714-07:00
model:                 gpt-4-0125-preview
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text in Dart involves examining strings to find certain patterns or sequences of characters and substituting them with new content. This operation is fundamental for tasks like data validation, formatting output, parsing user input, or even manipulating URLs and file paths, making applications more dynamic and responsive to user needs.

## How to:

Dart provides robust methods for searching and replacing text directly through its `String` class, without the need for external libraries. Here's how you can do it:

### Basic Searching and Replacing

To search for a substring and replace it with another string, you can use `replaceAll`:

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Output: Hello, Flutter! Flutter is great.
```

### Using Regular Expressions

For more complex searching and replacing needs, Dart utilizes regular expressions via the `RegExp` class. This allows for pattern matching and replacement in strings:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Output: Dart 2024, Flutter 2024
```

This example finds all instances of one or more digits (`\d+`) in the string and replaces them with "2024".

### Case-Insensitive Searching

To perform a case-insensitive search, you can modify the `RegExp` constructor to ignore the case:

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Output: Welcome to Flutter, the programming language.
```

### Replacing with a Function

For dynamic replacements based on the match itself, Dart allows passing a function to `replaceAllMapped`. This function can perform operations or calculations on the matched sequences:

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Output: Increment 6 by 1 to get 7.
```

This replaces each digit sequence with its increment value. Each match is parsed into an integer, incremented, and then converted back to a string for replacement.

Dart's string manipulation capabilities, particularly for searching and replacing text, make it a powerful tool for processing and preparing data within your applications. Whether using straightforward string replacements or leveraging the power of regular expressions, Dart provides the flexibility and performance needed for effective text manipulation.
