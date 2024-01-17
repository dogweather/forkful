---
title:                "Reading a text file"
html_title:           "Swift recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is the process of retrieving and displaying the contents of a file containing plain text, such as a document or code file. Programmers commonly read text files to extract data, manipulate and analyze it, or to use it as input for their programs.

## How to:

To read a text file in Swift, you can use the `String` class's `contentsOfFile` method. First, create a string variable to store the contents of the file, then use the `try` and `catch` keywords to handle any errors that may occur. Finally, print the contents of the file using the `print` function.

```
let filename = "example.txt" 
do {
  let fileContents = try String(contentsOfFile: filename)
  print(fileContents)
}
catch {
   print("Error: cannot readfile")
}
```

Running this code will result in the contents of the text file being printed in the console.

## Deep Dive:

Text files have been a popular way of storing and exchanging data since the early days of computing. They are lightweight and easily readable by both humans and computers, making them a versatile choice for data storage. Alternatives to text files include binary files, which are more compact but not easily human-readable, and database systems, which offer more advanced features but may be more complex to use.

When reading a text file in Swift, it is important to handle errors properly. The `try` and `catch` keywords allow you to handle exceptions that may occur while reading the file, such as the file not existing or not having the correct permissions. In addition to `String`'s `contentsOfFile` method, there are other ways to read and parse text files in Swift, such as using the `Data` class or third-party libraries.

## See Also:

For more information on reading text files in Swift, check out the official Apple documentation on file handling: https://developer.apple.com/documentation/foundation/file-management. You can also explore other methods and libraries for reading and parsing text files, such as `Scanner` and `Codable`.