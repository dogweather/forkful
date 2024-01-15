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

## Why
Checking if a directory exists in Swift may seem like a simple task, but it can be crucial when working with files and directories in your code. It ensures that you are not accidentally overwriting or creating duplicate directories.

## How To
To check if a directory exists in Swift, we can use the `FileManager` class. This class provides methods for managing files and directories in our app. Let's take a look at an example:

```Swift
let folderName = "Documents"
let fileManager = FileManager.default
let documentsDir = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0]
let folderPath = documentsDir.appendingPathComponent(folderName, isDirectory: true)

// Check if directory exists
if fileManager.fileExists(atPath: folderPath.path) {
    print("The \(folderName) directory exists.")
} else {
    print("The \(folderName) directory does not exist.")
}
```

In this example, we first define the name of the folder we want to check for. Then, we use `FileManager` to get the URL for the document directory of our app. We append our folder name to this URL and use the `fileExists(atPath:)` method to check if the directory exists. If it does, we print a message informing the user. Otherwise, we print a different message. 

## Deep Dive
The `FileManager` class also provides us with a few other useful methods for checking the existence of directories. These include `fileExists(atPath:)`, `fileExists(atPath:isDirectory:)`, and `fileExists(atPath:isDirectory:error:)`. Each of these methods takes in the path of the directory we want to check and returns a boolean value indicating whether it exists or not. The main difference between these methods is that the last two also allow us to specify whether we want to check if the path is a directory or not. This can be useful when working with different types of files and directories in our app. 

## See Also
- [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Working with Directories in Swift](https://medium.com/fabcoding/working-with-directories-in-swift-f45aa5504ce)