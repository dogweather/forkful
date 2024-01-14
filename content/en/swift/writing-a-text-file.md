---
title:                "Swift recipe: Writing a text file"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are an essential part of programming as they allow for the storage and manipulation of data. Learning how to create and write to a text file is a vital skill for any Swift programmer.

## How To
Creating and writing to a text file in Swift is a relatively straightforward process. First, we need to define the file path where we want our text file to be saved. We can do this by using the `FileManager` class and its `urls(for:in:)` method.

```
Swift
let fileManager = FileManager.default
let documentDirectory = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!
let filePath = documentDirectory.appendingPathComponent("myFile.txt")
```

Next, we need to create the actual text file by using the `createFile(atPath:contents:attributes:)` method. We can specify the content of the file as a `Data` object, which we can convert from a `String` using the `data(using:)` method.

```
Swift
let fileContent = "This is the content of my text file."
let fileData = fileContent.data(using: .utf8)
fileManager.createFile(atPath: filePath.path, contents: fileData, attributes: nil)
```

And that's it! We have successfully created and written to our text file.

## Deep Dive
When creating a text file, there are a few things to keep in mind. The first parameter of the `urls(for:in:)` method takes in a `SearchPathDirectory` enum which specifies the location of the file. In this example, we used `.documentDirectory` which is a directory meant for user-generated content. Other options include `.cachesDirectory` for cached resources and `.applicationSupportDirectory` for application-specific data.

Additionally, the `createFile(atPath:contents:attributes:)` method also takes in an `attributes` parameter, where we can specify the file's attributes such as its creation date and file size.

## See Also
- [Apple Developer Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Developer Documentation on NSData](https://developer.apple.com/documentation/foundation/nsdata)
- [Learn Swift by Codecademy](https://www.codecademy.com/learn/learn-swift)