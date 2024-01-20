---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file involves saving data as readable text, using a character encoding like UTF-8. Programmers do this for logging, data persistence, or configuration.

## How to:
Writing text to a file in Swift is straightforward with the `String` class and `FileManager`. Here's a quick example:

```Swift
import Foundation

let stringToWrite = "Hello, Swift!"
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("example.txt")

do {
    try stringToWrite.write(to: fileURL!, atomically: true, encoding: .utf8)
    print("File written successfully")
} catch {
    print("Error writing to file: \(error)")
}
```

Sample Output:
```
File written successfully
```

## Deep Dive
Writing text files is as old as computers themselves, often used for small data storage before databases became common. Key alternatives include databases and user defaults, which are structured and more efficient for larger data sets. When writing files in Swift, the `write(to:atomically:encoding:)` method ensures atomic writes, preventing data corruption during a write operation.

## See Also
- Swift String Documentation: https://developer.apple.com/documentation/swift/string
- FileManager Guide: https://developer.apple.com/documentation/foundation/filemanager
- Working with JSON in Swift: https://developer.apple.com/swift/blog/?id=37
- File Handling in Swift Tutorial: https://www.raywenderlich.com/1881-file-handling-in-swift-tutorial