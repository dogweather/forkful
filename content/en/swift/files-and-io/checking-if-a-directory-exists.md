---
date: 2024-02-03 19:02:41.389086-07:00
description: "How to: Swift's Foundation framework provides the `FileManager` class,\
  \ which has methods to manage the file system. You can use `FileManager` to check\
  \ if\u2026"
lastmod: '2024-03-13T22:45:00.408998-06:00'
model: gpt-4-0125-preview
summary: Swift's Foundation framework provides the `FileManager` class, which has
  methods to manage the file system.
title: Checking if a directory exists
weight: 20
---

## How to:
Swift's Foundation framework provides the `FileManager` class, which has methods to manage the file system. You can use `FileManager` to check if a directory exists. Here's a snippet on how to do this:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

However, this checks for both files and directories. If you specifically want to verify a directory exists, you need to pass a pointer to a Boolean value in `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

### Using a Third-Party Library
As of now, checking for the existence of a directory in Swift usually doesn’t necessitate third-party libraries due to the robustness of the `FileManager` class. However, for more complex file manipulation and checking, libraries like **Files** by John Sundell provide a more Swift-friendly API.

Here’s how you might use it:

First, add Files to your project via Swift Package Manager.

Then, you can check for a directory's existence like so:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Directory exists")
} catch {
    print("Directory does not exist")
}
```

Note: As third-party libraries can change, always refer to the latest documentation for usage and best practices.
