---
date: 2024-03-08 21:33:36.614571-07:00
description: "Writing to standard error (stderr) in Dart is about sending error messages\
  \ and diagnostics to a separate stream, distinct from the standard output\u2026"
lastmod: '2024-03-09T21:06:23.369136-07:00'
model: gpt-4-0125-preview
summary: "Writing to standard error (stderr) in Dart is about sending error messages\
  \ and diagnostics to a separate stream, distinct from the standard output\u2026"
title: Writing to standard error
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) in Dart is about sending error messages and diagnostics to a separate stream, distinct from the standard output (stdout). Programmers do this to differentiate between normal program output and errors or warning messages, allowing for easier debugging and logging.

## How to:

In Dart, writing to stderr is straightforward using the `stderr` object available in `dart:io`. Here’s a basic example:

```dart
import 'dart:io';

void main() {
  stderr.writeln('This is an error message.');
}
```

Output when run:
```
This is an error message.
```
This message is sent to the stderr stream, which is typically displayed in the console or terminal.

To demonstrate more complexity, such as logging an exception, Dart's rich set of features allows for concise and effective error handling:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simulate an operation that might throw
    throw Exception('Something went wrong!');
  } catch (e) {
    stderr.writeln('Error: $e');
  }
}

void main() {
  riskyOperation();
}
```

Output when run:
```
Error: Exception: Something went wrong!
```

This pattern is especially useful for applications that need to separate normal logs from error logs, making it easier to monitor and debug applications.

While Dart's standard library is quite comprehensive, many programs don't require third-party libraries for writing to stderr. However, if your application needs more sophisticated logging capabilities (e.g., to files, over the network, formatting), the `logging` package is a popular choice. Here's a quick peek at using `logging` for errors:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Severe Error: Something significantly bad happened.');
}
```

Output when run:
```
SEVERE: 2023-04-01 00:00:00.000: Severe Error: Something significantly bad happened.
```

This method offers a higher degree of customization and control over what gets logged as an error and how it's formatted, which can be very helpful in larger, more complex applications.
