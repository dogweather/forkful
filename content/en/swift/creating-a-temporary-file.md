---
title:                "Swift recipe: Creating a temporary file"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files is a useful technique in Swift programming for handling temporary data or files that are only needed temporarily. It allows for efficient and secure management of data without cluttering up the permanent file system.

## How To
To create a temporary file in Swift, we use the built-in `FileManager` class. First, we need to specify the `temporaryDirectory` property of the `FileManager` class as the destination for our temporary file.

```
let fileManager = FileManager.default
let temporaryDirectory = fileManager.temporaryDirectory
```

Next, we can use the `FileManager` class's `createFile` method to create our temporary file. This method takes in two parameters, the file's path and the content we want to write to the file.

```
let fileURL = temporaryDirectory.appendingPathComponent("tempFile.txt")
let content = "This is a temporary file created using Swift!"
try fileManager.createFile(atPath: fileURL.path, contents: content, attributes: nil)
```

To confirm that our temporary file has been created, we can use the `contents(atPath)` method of the `FileManager` class to read the contents of the file and print it out to the console.

```
let fileContents = try fileManager.contents(atPath: fileURL.path)
if let contents = fileContents {
    let contentsString = String(data: contents, encoding: .utf8)
    print(contentsString)
}
```

The output in the console should be the content we specified earlier - "This is a temporary file created using Swift!"

## Deep Dive
When creating a temporary file, it's important to consider how long the file will be needed for and how it will be used. This will help determine the appropriate method for creating the file.

One method is to use the `createFile` method as shown in the "How To" section. This method creates a file with a unique name in the specified directory. However, this file will not automatically be deleted and will remain in the temporary directory until manually deleted.

Another method is to use the `mkstemp()` function from the `stdlib` library. This function creates a temporary file with a unique name, but also returns a file descriptor that allows us to remove the file when it is no longer needed.

```
guard let template = temporaryDirectory.path + "/tempFile.XXXXXX" cString(using: .utf8) else {
    return
}
let fileDescriptor = mkstemp(template)
```

Once the file is no longer needed, we can use the `close()` function to close the file descriptor and delete the temporary file.

```
close(fileDescriptor)
```

## See Also
- Official Apple Documentation for `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Useful resource for working with temporary files in Swift: https://www.swiftdevcenter.com/temporary-file-directory-in-swift/
- Awesome Swift: A curated list of Swift resources: https://github.com/matteocrippa/awesome-swift