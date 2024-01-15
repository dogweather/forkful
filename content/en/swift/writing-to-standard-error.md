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

## Why

Writing to standard error is a useful skill for debugging and error handling in Swift programming. By being able to print error messages to the standard error stream, developers can easily identify and fix issues in their code.

## How To

In Swift, printing to standard error can be done using the `FileHandle` class. First, we need to import the Foundation framework at the top of our code:

```Swift
import Foundation
```

Then, we can create an instance of `FileHandle` for the standard error stream:

```Swift
let errorStream = FileHandle.standardError
```

To write to the standard error stream, we use the `write` method and pass in a `Data` object containing the message we want to print:

```Swift
// Example error message
let errorMessage = "Oops, something went wrong!"

if let data = errorMessage.data(using: .utf8) {
    errorStream.write(data)
}
```

The output of this code will be:

```
Oops, something went wrong!
```

## Deep Dive

In Swift, the standard error stream is a special output channel that is separate from the standard output stream. This allows developers to differentiate between regular program output and error messages.

Using the `FileHandle` class, we can also redirect the standard error stream to a file or another stream if needed. This can be useful for logging errors or debugging on different systems.

Additionally, we can customize the appearance of error messages by including escape sequences in the message string. For example, to print a message in red, we can use the escape sequence `\u{001B}[0;31m`, followed by our message and then the escape sequence `\u{001B}[0;0m` to reset the color. This may be helpful for visually highlighting errors in a console.

## See Also

- [Swift FileHandle Class](https://developer.apple.com/documentation/foundation/filehandle)
- [Standard Streams in Swift](https://www.hackingwithswift.com/articles/165/how-to-send-data-to-standard-input-and-output)
- [Error Handling in Swift](https://www.swiftbysundell.com/basics/error-handling/)