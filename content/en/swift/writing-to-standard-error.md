---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (`stderr`) channels critical messages to a special output stream reserved for errors, separate from the main output (`stdout`). Programmers use it for logging errors and diagnostic messages so they don't jumble up regular program data and can be easily tracked or redirected.

## How to:
Swift makes writing to `stderr` straightforward. See the example below:

```Swift
import Foundation

// Writing to standard error
func writeToStdErr(_ message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Example usage
writeToStdErr("Oops! Something went wrong.")

// Output when run in a console could look like this
// (although this won't be visible in Xcode's console):
// Oops! Something went wrong.
```

## Deep Dive
In earlier programming days, distinguishing between `stdout` (standard output) and `stderr` (standard error) was vital for parsing command output and handling errors. Other languages offer similar constructs, and in Unix-based systems, these streams relate directly to the terminal. Implementing this in Swift taps into the same underlying principles, where `stderr` serves as an unbuffered stream, meaning it immediately flushes the output. This behavior is crucial for real-time error reporting. 

Alternatives include logging frameworks that can offer more features, like log levels and message formats. Swift's own standard libraries are rather minimalistic; if you need sophistication, you'll likely look at third-party libraries or the Apple-unified logging system.

## See Also
For a deeper understanding and additional context, check out these resources:

- [Apple's Unified Logging documentation](https://developer.apple.com/documentation/os/logging)
- [Swift’s Standard Library reference for FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
