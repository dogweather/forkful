---
date: 2024-01-20 17:41:13.465045-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) Swift\
  \ has the `FileManager` class to help out. Here's a quick example."
lastmod: '2024-04-05T21:53:50.019435-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) Swift has the\
  \ `FileManager` class to help out."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## How to: (Як зробити:)
Swift has the `FileManager` class to help out. Here's a quick example:

```swift
import Foundation

func createTemporaryFile() {
    let tempDirectoryURL = FileManager.default.temporaryDirectory
    let tempFileURL = tempDirectoryURL.appendingPathComponent("tmpfile").appendingPathExtension("txt")
    
    let content = "Temporary data here"
    do {
        try content.write(to: tempFileURL, atomically: true, encoding: .utf8)
        print("Temporary file created at \(tempFileURL.path)")
    } catch {
        print(error)
    }
}

createTemporaryFile()
```

Sample output:

```
Temporary file created at /var/folders/.../tmpfile.txt
```

## Deep Dive (Поглиблений Розгляд):
Historically, temporary files date back to the early days of computing, used to manage limited storage space and reduce memory usage. Alternatives to temporary files include in-memory storage or databases, but these may be volatile or impractical for large data sets. In Swift, `FileManager` ensures that temp files go in the correct directory, which is unique per user and session, minimizing security risks and avoiding clashes.

## See Also (Дивіться Також):
- Swift `FileManager` documentation: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- Temporary file guidelines: [File System Basics](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)
- Working with files in Swift: [Read and write files](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-files-using-file-manager)
