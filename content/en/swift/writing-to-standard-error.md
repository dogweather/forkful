---
title:                "Swift recipe: Writing to standard error"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When writing code in Swift, it is important to be aware of different methods of handling and displaying errors. One common way to do this is by writing to standard error, which allows for separate handling and formatting of errors compared to regular print statements.

## How To

To write to standard error in Swift, we can use the `FileHandle.standardError` method. This method returns a `FileHandle` object that represents the standard error file descriptor. We can then use this object to write our error message using the `.write()` method, along with the `data(using:)` method to convert our string into `Data` format.

```Swift
let myErrorMessage = "Uh oh, something went wrong!"
let data = myErrorMessage.data(using: .utf8)
FileHandle.standardError.write(data!)
```

The output of this code would be the `myErrorMessage` string being displayed in the console with a red font, indicating it is an error message.

## Deep Dive

Writing to standard error may seem like a small aspect of Swift programming, but it can be extremely useful when handling different types of errors. By using standard error, we are able to differentiate between regular print statements and error messages, making it easier to debug our code.

Additionally, by writing to standard error, we are following best practices and adhering to established conventions in the Swift community. This helps to maintain consistency and readability in our code.

## See Also

For more information on writing to standard error in Swift, check out these resources:

- [Swift Standard Library Documentation](https://developer.apple.com/documentation/swift/filehandle)
- [Swift Style Guide](https://github.com/raywenderlich/swift-style-guide#standard-library)