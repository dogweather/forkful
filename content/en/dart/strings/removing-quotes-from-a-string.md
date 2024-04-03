---
date: 2024-03-08 21:33:57.864280-07:00
description: 'How to: Dart provides straightforward ways to remove quotes from a string
  using built-in string methods without the need for third-party libraries. #.'
lastmod: '2024-03-13T22:44:59.809645-06:00'
model: gpt-4-0125-preview
summary: Dart provides straightforward ways to remove quotes from a string using built-in
  string methods without the need for third-party libraries.
title: Removing quotes from a string
weight: 9
---

## How to:
Dart provides straightforward ways to remove quotes from a string using built-in string methods without the need for third-party libraries.

### Example 1: Using `replaceFirst` and `replaceAll`
If you're dealing with strings that start and end with quotes, you can use `replaceFirst` and `replaceAll` methods to remove them.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// Removing double quotes
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Output: Hello, World!

// Removing single quotes
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Output: Dart Programming
```

### Example 2: Using `substring`
This method is useful when you are sure the quotes are at the very start and end of the string.

```dart
String quotedString = '"Flutter Development"';
// Check if it starts and ends with quotes before removing to avoid errors
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Output: Flutter Development
```

### Example 3: Custom Extension Method
For more reusability, particularly if your project involves frequent quote removal, consider creating a custom extension on `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // Output: This is Dart
  print(singleQuoted.unquote()); // Output: This is awesome
}
```

These approaches should help you remove quotes from strings effectively in Dart, enhancing your data processing and preparation workflows.
