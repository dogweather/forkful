---
title:                "Writing to standard error"
html_title:           "Swift recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error is a way for programmers to display error messages in their code. It allows them to easily identify and troubleshoot issues without disrupting the normal output of the program.

## How to:
To write to standard error in Swift, use the `print()` function with the `to` parameter set to `.standardError`. Here's an example:

```Swift
print("Error: Something went wrong.", to: .standardError)
```

The error message will be displayed in the console with a designated error symbol next to it. Here's what the output would look like:

```
❌ Error: Something went wrong.
```

## Deep Dive:
The practice of writing to standard error has been around since the early days of programming. Initially, programmers would simply print error messages to the standard output, but this proved to be problematic as it would mix with the normal output of the program. Hence, the introduction of standard error as a dedicated channel for error messages.

An alternative to writing to standard error is using logging frameworks, such as Apple's own `os_log`. However, these frameworks can be more complex to set up and use compared to the simple `print()` function.

Behind the scenes, writing to standard error uses the `stderr` stream, which is part of the UNIX file descriptor system. This stream is used to output text specifically for error messages, while the `stdout` stream is reserved for normal program output.

## See Also:
- [Logging in Swift](https://developer.apple.com/documentation/os/logging)
- [Standard Streams in UNIX](https://en.wikipedia.org/wiki/Standard_streams)