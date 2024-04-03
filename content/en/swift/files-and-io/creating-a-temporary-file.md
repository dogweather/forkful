---
date: 2024-01-20 17:41:10.668868-07:00
description: "How to: Swift makes creating temporary files pretty easy, using the\
  \ `FileManager` class. Here\u2019s how you whip up a temp file and write some text\
  \ into it."
lastmod: '2024-03-13T22:45:00.413252-06:00'
model: gpt-4-1106-preview
summary: Swift makes creating temporary files pretty easy, using the `FileManager`
  class.
title: Creating a temporary file
weight: 21
---

## How to:
Swift makes creating temporary files pretty easy, using the `FileManager` class. Here’s how you whip up a temp file and write some text into it:

```Swift
import Foundation

// Create a temporary directory URL
let tempDirectoryURL = FileManager.default.temporaryDirectory

// Create a unique file name
let fileName = UUID().uuidString

// Construct the full file URL
let fileURL = tempDirectoryURL.appendingPathComponent(fileName)

// Sample text to write
let sampleText = "Hello, temporary world!"

do {
    // Write the text to the temporary file
    try sampleText.write(to: fileURL, atomically: true, encoding: .utf8)
    print("File created: \(fileURL)")
} catch {
    print("Failed to write to file: \(error)")
}

// Sample output:
// File created: file:///path/to/temp/directory/E0B4952E-5BEE-47E7-B5BB-DA5E6AF1EDC9
```

To read the file, just flip the script—here’s how:

```Swift
do {
    // Read the text from the temporary file
    let savedText = try String(contentsOf: fileURL, encoding: .utf8)
    print("File contents: \(savedText)")
} catch {
    print("Failed to read file: \(error)")
}

// Sample output:
// File contents: Hello, temporary world!
```

Clean up after yourself by deleting the temp file:

```Swift
do {
    // Delete the temporary file
    try FileManager.default.removeItem(at: fileURL)
    print("Temporary file deleted.")
} catch {
    print("Failed to delete file: \(error)")
}

// Sample output:
// Temporary file deleted.
```

## Deep Dive
Before `FileManager`, folks managed files in more cumbersome ways. Remember C’s `tmpfile()`? Swift's `FileManager` is a breath of fresh air in comparison: simple and modern.

Alternatives? Sure. You might use in-memory representations like `Data` or `String`, perfect for truly temporary data with limited size. Another route is to use a custom temp file manager for more control, but that’s typically overkill.

The nitty-gritty: `FileManager` uses the system's temp directory, which is cleaned out occasionally but not after each program run. Keep that in mind when it comes to security or sensitive data—clean up manually if necessary.

## See Also
Check these out for more dirt on handling files in Swift:
- [Apple's FileManager Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [NSHipster article on File Management](https://nshipster.com/temporary-files/)
- [Ray Wenderlich’s guide to working with the file system in Swift](https://www.raywenderlich.com/666-filemanager-class-tutorial-for-macos-getting-started)
