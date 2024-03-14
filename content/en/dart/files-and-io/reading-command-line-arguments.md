---
date: 2024-03-08 21:33:34.618536-07:00
description: "Reading command line arguments in Dart enables programmers to input\
  \ data directly into the console when executing a Dart program, enhancing its\u2026"
lastmod: '2024-03-13T22:44:59.834461-06:00'
model: gpt-4-0125-preview
summary: "Reading command line arguments in Dart enables programmers to input data\
  \ directly into the console when executing a Dart program, enhancing its\u2026"
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Dart enables programmers to input data directly into the console when executing a Dart program, enhancing its interactivity and flexibility for various use cases, including automation scripts, CLI tools, or batch processing. This feature is pivotal for creating adaptable and user-friendly command-line applications.

## How to:

Dart provides a straightforward approach to access command line arguments via the `List<String> args` in the main method. Below is a simple example demonstrating how to read and utilize command line arguments.

```dart
// main.dart
void main(List<String> args) {
  print('Command Line Arguments:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

To run this Dart program and pass command line arguments, use the Dart CLI like so:

```shell
dart run main.dart Hello World!
```

Expected output:

```
Command Line Arguments:
1: Hello
2: World!
```

### Using a Popular Third-Party Library: `args`

While Dart's built-in capabilities for handling command line arguments are robust for many applications, the `args` package provides a refined way to define and parse command line arguments for more complex needs.

First, add the `args` package to your `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Then, use it in your program as follows:

```dart
// Using the 'args' package
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hello, ${argResults['name']}!');
  } else {
    print('No name provided.');
  }
}
```

Run the program with a named argument:

```shell
dart run main.dart --name=John
```

Expected output:

```
Hello, John!
```

This simple introduction to parsing command line arguments, both natively and with the `args` library, showcases how Dart can handle user inputs right from the console, opening a pathway to creating more interactive and dynamic CLI applications.
