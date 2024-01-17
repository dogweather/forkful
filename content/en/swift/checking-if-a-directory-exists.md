---
title:                "Checking if a directory exists"
html_title:           "Swift recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# What & Why?
Checking if a directory exists means verifying if a particular location on your computer contains a folder or not. Programmers do this to ensure that their code runs smoothly and avoids errors when interacting with directories.

# How to:
To check if a directory exists in Swift, we can use the `FileManager` class and its `fileExists(atPath:isDirectory:)` method. This method takes in the path of the directory and a boolean value indicating if the path is a directory or not, and returns a boolean value indicating if the directory exists or not.

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/John/Documents"
var isDirectory: ObjCBool = true
let directoryExists = fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory)
print(directoryExists) // true
```

# Deep Dive:
Prior to Swift 4, there was no specific way to check if a directory exists. Programmers had to use the `fileExists(atPath:)` method and check if the path leads to a file or a directory by using the `isDirectory` property of the `FileManager` class. However, since Swift 4, the `fileExists(atPath:isDirectory:)` method was introduced, making it easier to check specifically for directories.

An alternative way to check if a directory exists is by using the `URL` and `DirectoryEnumerationOptions` classes. This method allows for more fine-grained control over the enumeration of files and folders within the directory.

```Swift
let directoryURL = URL(fileURLWithPath: "/Users/John/Documents")
let directoryExists2 = directoryURL.hasDirectoryPath
print(directoryExists2) // true
```

# See Also:
- [Apple's documentation on FileManager class](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple's documentation on URL class](https://developer.apple.com/documentation/foundation/url)
- [Swift's implementation of the FileManager class](https://github.com/apple/swift-corelibs-foundation/blob/main/Foundation/FileManager.swift)