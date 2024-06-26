---
date: 2024-03-08 21:33:23.045462-07:00
description: "How to: Dart does not come with an inbuilt REPL. However, you can achieve\
  \ REPL-like functionality using the DartPad (online) or by utilizing third-party\u2026"
lastmod: '2024-03-13T22:44:59.822263-06:00'
model: gpt-4-0125-preview
summary: Dart does not come with an inbuilt REPL.
title: Using an interactive shell (REPL)
weight: 34
---

## How to:
Dart does not come with an inbuilt REPL. However, you can achieve REPL-like functionality using the DartPad (online) or by utilizing third-party tools like `dart_repl`.

**Using DartPad:**

DartPad (https://dartpad.dev) is an online Dart editor that lets you write and run Dart code in your web browser. Although not a traditional command-line REPL, it provides a similar experience for rapid experimentation.

Simply go to the website, type your Dart code on the left pane, and click "Run" to see the output on the right.

Example:
```dart
void main() {
  print('Hello, Dart!');
}
```
Output:
```
Hello, Dart!
```

**Using `dart_repl` (third-party tool):**

First, install `dart_repl` via pub globally:

```shell
dart pub global activate dart_repl
```

Then, run `dart_repl` from your terminal:

```shell
dart_repl
```

Now, you can start typing Dart statements directly into the shell. For example:

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

These methods provide a swift pathway for trying out Dart code on-the-fly, significantly easing the learning curve and enhancing productivity.
