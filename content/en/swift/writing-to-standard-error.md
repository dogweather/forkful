---
title:                "Swift recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In Swift, writing to standard error can be a useful tool for debugging and troubleshooting your code. It allows you to print out error messages and other information while your program is running, which can help you identify and fix any issues.

## How To

To write to standard error in Swift, you can use the `print()` function and specify `StandardError` as the `terminator` parameter. Here's an example of how you can do this:

```Swift
print("This is an error message", terminator: .standardError)
```

This will print out the message "This is an error message" to your console's standard error stream.

You can also use the `fputs()` function to write to standard error. Here's an example:

```Swift
fputs("This is an error message", stderr)
```

This will have the same effect as using the `print()` function.

## Deep Dive

In Swift, the standard error stream is represented by `FileHandle.standardError`. This is a `FileHandle` object that allows you to read from and write to the standard error stream. You can use methods such as `readDataToEndOfFile()` and `write(_:)` to interact with this stream.

It's important to note that writing to standard error does not automatically terminate your program, unlike writing to standard output. This means that you can write multiple error messages throughout your program without interrupting its execution.

## See Also

- [Apple Developer Documentation on Standard Error](https://developer.apple.com/documentation/foundation/filehandle/standarderror)
- [Swift Programming Language Guide on Printing to Standard Error Stream](https://docs.swift.org/swift-book/LanguageGuide/Printing.html#ID390)
- [Stack Overflow Q&A on using Standard Error in Swift](https://stackoverflow.com/questions/26198526/swift-print-to-stderr)