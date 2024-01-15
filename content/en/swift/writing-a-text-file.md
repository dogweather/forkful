---
title:                "Writing a text file"
html_title:           "Swift recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming and can be useful for tasks such as saving user data or exporting information. In Swift, writing a text file is a simple and straightforward process that can be easily implemented in your projects.

## How To

To write a text file in Swift, we first need to create a file path to specify where the file will be saved. We can do this by using the `NSFileManager` class and the `URL` struct.

```Swift
let fileManager = FileManager.default
var filePath = fileManager.homeDirectoryForCurrentUser.appendingPathComponent("myFile.txt")
```

Next, we need to specify the content that we want to write in our text file. For this example, we will write a simple message.

```Swift
let message = "Hello, world!"
```

Then, we can use the `write(to:atomically:encoding)` method of the `NSString` class to write the content to our file path.

```Swift
do {
    try message.write(to: filePath, atomically: true, encoding: .utf8)
} catch {
    print("Error writing file: \(error)")
}
```

The `atomically` parameter allows us to specify whether the file should be saved atomically, meaning it is either saved in its entirety or not at all. The `encoding` parameter specifies the text encoding we want to use for our file, in this case, UTF-8.

If the file is successfully written, we should see a new text file with our message in the specified file path.

## Deep Dive

The `write(to:atomically:encoding)` method is provided by the `NSString` class, but can also be used with the `Data` class. This allows us to write binary data to our text file if needed.

We can also use the `append(toFile:)` method to append content to an existing text file. This is useful for situations where we want to add information to a file without overwriting its existing content.

```Swift
let fileManager = FileManager.default
var filePath = fileManager.homeDirectoryForCurrentUser.appendingPathComponent("myFile.txt")

let message = "Hello, world!"

// Write message to file
do {
    try message.write(to: filePath, atomically: true, encoding: .utf8)
} catch {
    print("Error writing file: \(error)")
}

let newMessage = "This is a new message."

// Append new message to file
if let fileHandle = FileHandle(forWritingAtPath: filePath.path) {
    fileHandle.seekToEndOfFile()
    fileHandle.write(newMessage.data(using: .utf8)!)
    fileHandle.closeFile()
}
```

Another important thing to keep in mind is that when writing to a file, we must wrap our code in a `do-catch` block as it can potentially throw an error.

## See Also

- [Swift Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [NSFileManager Class Reference](https://developer.apple.com/documentation/foundation/nsfilemanager)
- [NSString Class Reference](https://developer.apple.com/documentation/foundation/nsstring)
- [Data Class Reference](https://developer.apple.com/documentation/foundation/data)