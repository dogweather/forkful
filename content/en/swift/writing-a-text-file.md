---
title:    "Swift recipe: Writing a text file"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

In today's digital world, writing a text file may seem like a thing of the past. However, it still serves an important purpose in many applications. Whether you need to store simple data, transfer information between systems, or even just create a log, knowing how to write a text file can be a valuable skill for any Swift programmer.

## How To

Writing a text file in Swift is a simple process. First, we need to define the file path where we want to save our text file. We can do this by using the `URL` class and specifying the location and name of our file.

```Swift
let fileURL = URL(fileURLWithPath: "myTextFile.txt")
```

Next, we need to create a string with the contents we want to write to our text file. This can be anything from a single word to a large block of text.

```Swift
let text = "Hello, world!"
```

Now, we can use the `write(to:atomically:encoding:)` method to actually write the text to our file. This method takes in the file URL, a boolean value determining whether we want to write atomically (meaning the entire file is written at once), and an encoding value.

```Swift
do {
    try text.write(to: fileURL, atomically: true, encoding: .utf8)
    print("Text file successfully created!")
} catch {
    print("Error writing to file: \(error)")
}
```

When we run this code, we should see our text file appear in the specified location with the text "Hello, world!" written inside.

## Deep Dive

The `write(to:atomically:encoding:)` method is just one way to write a text file in Swift. Another option is to use the `FileManager` class, which allows us to create the file if it doesn't already exist and append text to it. We can also specify a different encoding, such as UTF-16 or ASCII. Additionally, if we want to add more complex data, we can use the `NSKeyedArchiver` class to convert our data into an archived file, which can then be written to a text file.

## See Also

- [File System Programming Guide](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)
- [Encoding in Swift](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_data_with_coding/keyed_encoding)