---
title:                "Swift recipe: Checking if a directory exists"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Before diving into the "how" of checking if a directory exists in Swift, let's first discuss why someone might need to do this. In programming, it's important to be able to validate certain conditions in order to ensure that our code runs smoothly. In the case of checking if a directory exists, it allows us to handle potential errors and exceptions in our code.

## How To

To check if a directory exists in Swift, we will be using the `FileManager` class. This class provides us with a method called `fileExists(atPath:)` which takes in a path string as its parameter. Here's an example of how we can use this method:

```Swift
let fileManager = FileManager.default
let path = "/Users/John/Documents/Projects"
if fileManager.fileExists(atPath: path) {
    print("The directory exists.")
} else {
    print("The directory does not exist.")
}
```

In this code, we first create an instance of `FileManager` and then specify the path we want to check. The `fileExists(atPath:)` method returns a boolean value, so we can use an if-else statement to handle the different outcomes.

Let's also take a look at the output we would see for each scenario:

- If the directory exists, the output would be: `The directory exists.`
- If the directory does not exist, the output would be: `The directory does not exist.`

It's also worth noting that the `fileExists(atPath:)` method not only checks for the existence of directories, but also files. So if we pass in a file path instead, it will perform the same task.

## Deep Dive

Now that we know how to check if a directory exists in Swift, let's dig a little deeper into how this method works. Under the hood, `fileExists(atPath:)` uses the `access()` system call to determine the existence of a file or directory. This call is provided by the underlying operating system and returns a boolean value depending on whether the file or directory exists or not.

One thing to keep in mind is that the `fileExists(atPath:)` method does not check if the path provided leads to a valid file or directory. It simply checks for the existence of that path.

## See Also
- [Apple Developer Documentation: FileManager class](https://developer.apple.com/documentation/foundation/filemanager)
- [SwiftDoc: FileManager.fileExists(atPath:)](https://swiftdoc.org/v3.0/type/FileManager/#func-fileexists)
- [Hacking with Swift: Checking for the existence of a file](https://www.hackingwithswift.com/example-code/system/how-to-check-for-the-existence-of-a-file-using-filemanager)

By understanding why and how to check if a directory exists in Swift, we can effectively handle potential errors in our code. As always, don't hesitate to consult the documentation or other resources if you have further questions or need additional clarification. Happy coding!