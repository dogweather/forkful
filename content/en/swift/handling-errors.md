---
title:                "Handling errors"
date:                  2024-01-21T21:19:52.546193-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "Swift"
category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors in Swift means anticipating and responding to problems that pop up when your code runs. We do it to control the chaos—keeping apps from crashing and giving the user a smooth experience.

## How to:
Swift uses error handling with `do`, `try`, and `catch` blocks. Let’s take a look:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Pretend we have some logic here to check if a file exists and if we have permission to read it
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "File content goes here"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Whoops! File not found.")
} catch FileError.noPermission {
    print("Ah! No permission to read the file.")
} catch {
    print("An unknown error occurred.")
}

```

Sample Output:

```
Whoops! File not found.
```

## Deep Dive
Error handling wasn't always as swish as it is now. In Objective-C, you'd deal with pointers to NSError objects, which felt clunky. Now, we have a more elegant system with Swift enums and the `Error` protocol.

Swift’s `throw` lets us signal something's gone wonky. `do` blocks act like error-aware realms, `try` prefix calls the risky business, and `catch` handles things if they go south.

Optionals are an alternative for situations that aren't quite "error" status but might still have "no result". They're a bit like Schrödinger's variables—they've got a value or they don't.

For real depth, check out `Result` types, which are snazzy hybrids between regular-return and error patterns.

## See Also
- Official Swift Error Handling Guide: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Swift Error Handling Best Practices: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Advanced Error Handling in Swift: [Medium Article](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)