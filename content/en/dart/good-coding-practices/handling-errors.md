---
title:                "Handling errors"
date:                  2024-03-08T21:33:51.623284-07:00
model:                 gpt-4-0125-preview
---

{{< edit_this_page >}}

## What & Why?
Handling errors in Dart is about anticipating and managing exceptions that arise during program execution to enhance reliability and usability. Programmers implement error handling to prevent crashes and provide meaningful feedback to users, ensuring a smoother, safer application experience.

## How to:
Dart supports two types of errors: *compile-time* errors and *run-time* errors. Compile-time errors are detected by the Dart analyzer before the code runs, whereas run-time errors, or exceptions, occur during execution. Here's how you handle exceptions in Dart:

### Try-Catch
Use `try-catch` to capture exceptions and prevent them from crashing your application:

```dart
try {
  var result = 100 ~/ 0; // Attempting division by zero, throws an exception
} catch (e) {
  print('Caught an exception: $e'); // Handles the exception
}
```
Sample output: `Caught an exception: IntegerDivisionByZeroException`

### Specific Exception
To handle specific exceptions, mention the exception after `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Cannot divide by zero.'); // Specifically handles dividing by zero exceptions
}
```
Sample output: `Cannot divide by zero.`

### Stack Trace
To get a stack trace for debugging, use a second parameter in the catch block:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Exception: $e');
  print('Stack trace: $s'); // Prints stack trace for debugging
}
```

### Finally
Use `finally` to execute code after try/catch, regardless of whether an exception was thrown:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Caught an exception: $e');
} finally {
  print('This is always executed.'); // Cleanup code or final steps
}
```
Sample output:
```
Caught an exception: IntegerDivisionByZeroException
This is always executed.
```

### Third-Party Libraries
Although Dart's core library is robust for error handling, you can also use third-party packages like `dartz` for functional programming which introduces concepts like `Either` and `Option` that can be used for error handling. Here’s an example using `dartz` for error handling:

1. Add `dartz` to your `pubspec.yaml` file under dependencies:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Use `Either` for handling errors gracefully in your Dart code:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('Cannot divide by zero.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Error: $left'), 
    (right) => print('Result: $right')
  );
}
```
Sample output: `Error: Cannot divide by zero.`

The `Left` part usually represents the error, and the `Right` part represents success. This pattern allows handling errors in a more functional way, offering clarity and control over error management.
