---
title:                "Swift recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
"One of the fundamental principles of programming is ensuring that our code is error-free and handles unexpected scenarios. In Swift, checking if a directory exists is an essential task to ensure that our code functions properly and efficiently."

## How To
```Swift
// Import the Foundation framework
import Foundation

// Create a FileManager instance
let fileManager = FileManager.default

// Specify the directory path to check
let documentsURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!

// Check if the directory exists
var isDirectory: ObjCBool = true
if fileManager.fileExists(atPath: documentsURL.path, isDirectory: &isDirectory) {
    // Directory exists
    print("The specified directory exists!")
} else {
    // Directory does not exist
    print("The specified directory does not exist.")
}

// Output: The specified directory exists!
```

## Deep Dive
In Swift, we use the `FileManager` class to check if a directory exists. The `fileExists(atPath:isDirectory:)` method takes in the directory path as the first parameter and a `Bool` value as the second parameter. This `Bool` value is passed as a pointer to the function and will be set to `true` if the specified path is a directory, and `false` if it is a file.

It is important to note that the `fileExists(atPath:)` method also checks for the existence of files, so we have to use the `isDirectory` parameter to differentiate between directories and files. If the path does not exist or cannot be accessed, the method will return `false`.

## See Also
- [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Checking if a File Exists in Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-exists-using-filemanager)
- [Understanding Pointers in Swift](https://learnappmaking.com/pointers-value-references-swift/)

Overall, checking if a directory exists is a simple but crucial task for Swift programmers. By utilizing the `fileExists(atPath:)` method and the `isDirectory` parameter, we can ensure that our code is error-free and effectively handle different scenarios.