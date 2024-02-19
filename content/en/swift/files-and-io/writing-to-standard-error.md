---
aliases:
- /en/swift/writing-to-standard-error/
date: 2024-02-03 19:03:49.305565-07:00
description: "Writing to standard error (stderr) is about directing your program's\
  \ error messages or diagnostics output to a separate stream, distinct from the standard\u2026"
lastmod: 2024-02-18 23:09:11.412627
model: gpt-4-0125-preview
summary: "Writing to standard error (stderr) is about directing your program's error\
  \ messages or diagnostics output to a separate stream, distinct from the standard\u2026"
title: Writing to standard error
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) is about directing your program's error messages or diagnostics output to a separate stream, distinct from the standard output (stdout). This is crucial for debugging and logging errors without cluttering the standard output, facilitating both developers' and users' understanding of the program's state and issues.

## How to:

In Swift, writing to standard error can be done using the `FileHandle` class for direct stderr access. Here's a simple example:

```swift
import Foundation

// Define a message
let errorMessage = "An error occurred.\n"

// Convert the message to data
if let data = errorMessage.data(using: .utf8) {
    // Write the error message to stderr
    FileHandle.standardError.write(data)
}
```

Output to stderr (typically viewed in a console or terminal):
```
An error occurred.
```

For more complex logging or when working with external libraries, one might consider using a third-party library like **SwiftLog**. Although **SwiftLog** doesn't write to stderr directly out of the box, you can implement a custom logging backend to achieve this. Here's a simplified example of defining a custom log handler that writes to stderr:

First, add **SwiftLog** to your project dependencies in `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Then, implement a custom log handler that writes to stderr:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// Usage
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("This is an error message")
```

Output to stderr:
```
This is an error message
```

This custom handler allows you to route your SwiftLog error messages directly to standard error, integrating seamlessly with other log messages your application might generate.
