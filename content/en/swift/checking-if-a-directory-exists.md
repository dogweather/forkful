---
title:    "Swift recipe: Checking if a directory exists"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
In Swift programming, it's important to check if a directory exists before performing certain actions. This helps ensure that the program is working with the correct file path and prevents any unexpected errors.

## How To
To check if a directory exists in Swift, we can use the `FileManager` class. It has a method called `fileExists(atPath:)` which takes in a file path as a parameter and returns a boolean value indicating if the file exists or not.

Here's an example code that checks if a directory named "Images" exists at the designated file path and prints the result:

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Desktop/Images" // replace with desired file path

if fileManager.fileExists(atPath: path) {
    print("The directory exists!")
} else {
    print("The directory does not exist.")
}
```

Sample output:
```
The directory exists!
```

## Deep Dive
When using the `fileExists(atPath:)` method, it's important to note that it only checks if a file or directory exists at the exact file path specified. This means if there is a small difference in the file path, the method will return `false`.

One way to ensure the correct file path is to use the `standardizedFileURL` property on the file path before passing it into the method. This will eliminate any extraneous characters from the path and compare it accurately.

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Desktop/Images" // replace with desired file path

if fileManager.fileExists(atPath: path.standardizedFileURL.path) {
    print("The directory exists!")
} else {
    print("The directory does not exist.")
}
```

## See Also
- [Apple Developer Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Stack Overflow post on checking if a directory exists in Swift](https://stackoverflow.com/questions/44569380/how-can-i-check-the-existence-of-a-directory-in-swift)
- [Tutorial on working with directories in Swift](https://www.raywenderlich.com/148034/grand-central-dispatch-tutorial-swift-3-part-2#toc-anchor-004)