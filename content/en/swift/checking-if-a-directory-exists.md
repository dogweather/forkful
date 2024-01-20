---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

We often need to verify if a directory exists in our file system, because if we try to manipulate a non-existent directory, we risk crashes. To ensure smooth execution of our code, swift developers often perform checks to confirm if a directory is present before proceeding with operations.

## How To:

You can check for directory existence using Swift’s FileManager. You fetch documents directory and then append the path of the directory to check.

```swift
import Foundation

let fileManager = FileManager.default
let documentsPath = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0]
let directoryPath = documentsPath.appendingPathComponent("myDirectory")

var isDir: ObjCBool = false
if fileManager.fileExists(atPath: directoryPath.path, isDirectory: &isDir) {
  if isDir.boolValue {
    // file exists and is a directory
    print("Directory exists")
  } else {
    // file exists and is not a directory
    print("Found a file but not a directory.")
  }
} else {
  //file does not exist
  print("Directory does not exist.")
}
```

Executing this snippet would print "Directory does not exist." if "myDirectory" is not present in your app's documents directory.

## Deep Dive:

This method of checking for directory existence has been around in some form since the early days of UNIX. There are other means of achieving the same result, like using the shell command `ls`, but doing so would be overkill and less secure for such a simple task.

When using third-party libraries like Alamofire, you have more explicit ways to check for directories, but why add a huge library just to perform this small task? You can also use `try?` to check if you can list the directory contents, but that would raise the same exceptions as if you tried to use the directory directly. The method we've used above is straight-up efficient, fast, and error-free.

The `fileExists(atPath: isDirectory:)` method is given a path to check and an optional reference to a Boolean value. This Boolean is set to `true` if the path is a directory and `false` if the file at the path isn't a directory. If the file at the path doesn't exist at all, the `fileExists(atPath: isDirectory:)` method simply returns `false`, and the value of isDirectory remains unchanged.

## See Also:

Dive in deep with official docs and other related sources:
- [The Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Apple Developer Documentation](https://developer.apple.com/documentation) 
- [Swift Forums - Apple Developer](https://developer.apple.com/forums/)