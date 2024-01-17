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

## What & Why?
Writing a text file refers to the act of creating or editing a file with plain text content. This is often done by programmers to store data or information that needs to be quickly and easily accessed, without the need for complex formatting. Text files are lightweight, easy to read, and can be opened using a simple text editor, making them a popular choice for organizing and storing data.

## How to:
To write a text file in Swift, we can use the built-in FileManager class. First, we need to create a string with the content we want to write to the file. Then, we specify the file path where we want to save the file. Finally, we use the FileManager's createFile() method to create the file and write the content to it. See the code below for an example:

```Swift
let content = "This is some text that we want to write to our file."
let filePath = "/Users/username/Desktop/test.txt"

let fileManager = FileManager.default
fileManager.createFile(atPath: filePath, contents: content.data(using: .utf8), attributes: nil)
```

This will create a file named "test.txt" on the desktop with the specified content.

## Deep Dive:
Text files have been around since the early days of computing and have remained a popular choice for storing data due to their simplicity and versatility. In addition to being easily readable by humans, text files can also be parsed and manipulated by programs, making them a useful tool for data storage and transfer. Alternatives to text files include binary files, which use a more complex and compact representation of data and are more suitable for larger or more complex data sets.

Writing a text file may seem like a simple task, but it involves multiple steps and considerations. For example, we may need to handle errors when creating the file, or ensure proper encoding so that the text file can be read correctly on different systems. It is also important to consider the file path where we want to save the file and ensure that it is accessible and writable.

## See Also:
To learn more about writing text files in Swift, check out the following resources:
- [Apple's official documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [A tutorial on how to read and write text files in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-strings-in-files)