---
date: 2024-03-08 21:33:23.280875-07:00
description: "Getting the current date in Dart involves querying the system for the\
  \ current date and time. This functionality is commonly used in applications for\u2026"
lastmod: '2024-03-13T22:44:59.829894-06:00'
model: gpt-4-0125-preview
summary: Getting the current date in Dart involves querying the system for the current
  date and time.
title: Getting the current date
weight: 29
---

## What & Why?
Getting the current date in Dart involves querying the system for the current date and time. This functionality is commonly used in applications for features like timestamping events, showing the current date to users, or calculating durations. Knowing how to efficiently retrieve and manipulate the current date is foundational for scheduling, logging, and time-sensitive features.

## How to:
Dart's core library provides straightforward access to the current date and time through the `DateTime` class. Here's the basic example to get the current date:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Example output: 2023-04-12 10:00:00.000
}
```

If you only need the date part (year, month, day), you can format the `DateTime` object:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Example output: 2023-04-12
}
```

Dart doesn't include a built-in library for more complex date formatting, but you can use the `intl` package for this purpose. First, add the package to your `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Then, you can format dates easily:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Example output: 2023-04-12
}
```

For more advanced formatting options, explore the `DateFormat` class provided by the `intl` package, which supports a wide range of patterns and locales.
