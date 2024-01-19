---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file in Swift allows you to store binary or text data for short-term processing. Programmers do this when they need transient, disposable data storage to manage memory usage or to share data between different parts of an application without cluttering the persistent storage.

## How To:

Here's how you can create a temporary file in Swift. Throw this snippet in your own function or playground to make a temporary file:

```Swift
import Foundation

let fileURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent(NSUUID().uuidString)

do {
    try "Hello World!".write(to: fileURL, atomically: true, encoding: String.Encoding.utf8)
} catch {
    print("Failed writing to URL: \(fileURL), Error: " + error.localizedDescription)
}
print("File Path: \(fileURL.path)")
```

This will create and write "Hello World!" to a temporary file. The console will print the file path:

```Swift
File Path: /var/folders/.../temp_uuid
```

## Deep Dive

Historically, websites used session cookies to store temporary data. Today, we have the power of Swift's Foundation framework to manage temporary files locally. This non-persistent storage improves application performance by reducing read/write operations on the main storage disk.

However, you aren't tied to just files! You can use Swift's `UserDefaults` for simpler, infrequent temporary data storage needs. It's easier but less secure and less suitable for larger data.

The `NSTemporaryDirectory()` function returns a string path to the system's temporary directory. The `NSUUID().uuidString` generates a unique identifier for the file name. The `write(to:atomically:encoding:)` method then writes directly to the file at the path.

## See Also

For more information on managing files and directories with Swift, check out Apple's documentation: [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)

To understand `URL` in detail, head over to: [Apple Developer Documentation: URL](https://developer.apple.com/documentation/foundation/url)

Learn about the ephemeral nature of temporary files: [Creating Temporary Files in Swift](https://stackoverflow.com/questions/30743408/check-for-file-and-delete-it-swift-programming)