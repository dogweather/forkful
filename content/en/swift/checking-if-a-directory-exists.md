---
title:    "Swift recipe: Checking if a directory exists"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

In Swift programming, checking if a directory exists is an important feature to ensure proper handling of files and folders. It allows us to verify if a certain directory is present before attempting to read, write or delete any contents within it. This helps in avoiding any potential errors or crashes in our code.

## How To

To check if a directory exists in Swift, we can use the `FileManager` class and its `fileExists(atPath:)` method. This method takes in a file path as a parameter and returns a boolean value indicating whether the file or directory exists or not.

```Swift
let fileManager = FileManager.default

// Specify the file path
let filePath = "/Users/username/Documents/MyDirectory"

// Check if the directory exists
if fileManager.fileExists(atPath: filePath) {
    print("The directory exists")
} else {
    print("The directory does not exist")
}
```

In the above code, we first create an instance of the `FileManager` class and then specify the path to the directory that we want to check. Next, we use the `fileExists(atPath:)` method to determine if the directory exists or not. If it does, the message "The directory exists" will be printed, otherwise, "The directory does not exist" will be printed.

We can also use the `url(for:in:appropriateFor:create:)` method to get the file or directory's URL and then check for its existence using the `checkResourceIsReachable()` method. This method returns a boolean value similar to `fileExists(atPath:)` method.

```Swift
// Get the URL for the directory
if let dirURL = fileManager.url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: false) {
    // Check if the directory exists
    if fileManager.checkResourceIsReachable(at: dirURL) {
        print("The directory exists")
    } else {
        print("The directory does not exist")
    }
}
```

## Deep Dive

Behind the scenes, the `fileExists(atPath:)` and `checkResourceIsReachable()` methods use the underlying file system to check for the existence of a directory. These methods can be used for both local directories within the app's sandbox and remote directories on a network server.

It is important to note that these methods only check for the existence of a directory and not its contents. We can use `isReadableFile(atPath:)` and `isWritableFile(atPath:)` methods to determine if we have the appropriate privileges to access and modify the contents within the directory.

## See Also

For more information on handling files and directories in Swift, check out these helpful resources:

- [Apple Developer Documentation: FileManager class](https://developer.apple.com/documentation/foundation/filemanager)
- [Code Tuts+: Working with Directories in Swift](https://code.tutsplus.com/tutorials/working-with-directories-in-swift--cms-29818)
- [Swift.org: Handling Files and Directories](https://swift.org/blog/os-x-filesystem-interaction/)