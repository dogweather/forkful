---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:58:27.018385-07:00
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
In Swift, checking if a directory exists helps you confirm a file system's state before you read or write data. Programmers do this to avoid errors, like reading from a non-existent directory, which can crash an app or lead to faulty operations.

## How to:
Swift's `FileManager` has the tools for this. Use its `fileExists(atPath:)` method:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/directory"

if fileManager.fileExists(atPath: path) {
    print("Yep, it's there!")
} else {
    print("Nope, doesn't exist.")
}
```

Sample output if the directory exists:

```
Yep, it's there!
```

Or if it doesn’t:

```
Nope, doesn't exist.
```

## Deep Dive
Before `FileManager`, which came with the Foundation framework, UNIX commands in scripts were common for checking paths. But `FileManager` is easier and safer. Alternatives in Swift include working with the `URL` class and its `checkResourceIsReachable()` method, though it's more suitable for checking file availability and can throw errors. Internally, `FileManager` uses the `stat` system call to verify the existence of a path without regard to whether it’s a file or a directory, so when you need to differentiate, you'll have to further inspect the path's attributes.

## See Also
- Swift Documentation: [`FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- Swift Book: [Working with Directories](https://docs.swift.org/swift-book/)
- Apple Developer Forums: [File System Access](https://developer.apple.com/forums/tags/file-system/)
