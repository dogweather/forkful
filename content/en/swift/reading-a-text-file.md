---
title:                "Swift recipe: Reading a text file"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading and writing text files is a fundamental task in programming and can be essential when working with data. Whether you need to parse user input, save data to a file, or process a large dataset, knowing how to read a text file efficiently is a valuable skill for any Swift programmer.

## How To

To read a text file in Swift, we first need to specify the file path and create a File Handle object. We can then use the `readDataToEndOfFile()` method to read the contents of the file into a `Data` object. Finally, we can convert the `Data` into a `String` for further processing.

```Swift
// Specify file path
let filePath = "/Users/username/filename.txt"

do {
  // Create a File Handle object
  let fileHandle = FileHandle.init(forReadingAtPath: filePath)

  // Read file contents into Data object
  let fileData = try fileHandle?.readDataToEndOfFile()

  // Convert Data to String
  let fileContent = String(data: fileData, encoding: .utf8)

  // Print string
  print(fileContent)
} catch {
  print(error.localizedDescription)
}
```

#### Sample Output

```
Hello world! This is a text file.
It contains multiple lines of text.
We can read and process this file using Swift programming.
```

## Deep Dive

Using the `readDataToEndOfFile()` method to read the entire file into a `Data` object is efficient for small text files. However, for large files, it may not be the most memory-efficient solution. In such cases, we can use the `readabilityHandler` property of the File Handle object to read the file line by line, instead of loading the entire file into memory at once.

```Swift
let filePath = "/Users/username/filename.txt"

do {
  // Create a File Handle object
  let fileHandle = FileHandle.init(forReadingAtPath: filePath)

  // Set readabilityHandler to process file line by line
  fileHandle?.readabilityHandler = { fileHandle in
    if let line = String(data: fileHandle.availableData, encoding: .utf8) {
      // Process each line of the file here
      print(line)
    } else {
      print("Unable to read data.")
    }
  }
} catch {
  print(error.localizedDescription)
}
```

## See Also

- [Apple Developer Documentation: FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Hacking with Swift: Reading from disk with contentsOf](https://www.hackingwithswift.com/example-code/strings/reading-from-disk-with-contentsof)
- [Swift by Sundell: Working with files and folders in Swift](https://www.swiftbysundell.com/articles/working-with-files-and-folders-in-swift/)