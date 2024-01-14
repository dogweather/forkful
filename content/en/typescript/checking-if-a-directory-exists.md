---
title:                "TypeScript recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When writing a program, it is important to ensure that all necessary files and directories exist in order to avoid any unexpected errors. This is especially true when working with directories, as they can contain important data or serve as a crucial part of the program's functionality. In TypeScript, checking if a directory exists is a simple but essential task that can prevent potential issues down the line.

## How To

To check if a directory exists in TypeScript, we can use the `fs` module from Node.js. First, we need to import the module using the following code:

```TypeScript
import * as fs from 'fs';
```

Next, we can use the `existsSync()` method provided by the `fs` module. This method takes in the path of the directory we want to check as a parameter and returns a boolean value indicating whether the directory exists or not. Here is an example of how we can use this method:

```TypeScript
const directoryPath = './myDirectory';
if (fs.existsSync(directoryPath)) {
  // The directory exists
  console.log('Directory exists!');
} else {
  // The directory does not exist
  console.log('Directory does not exist!');
}
```

Alternatively, we can also use the `accessSync()` method which performs the same check but throws an error if the directory does not exist. Here is an example of using this method:

```TypeScript
try {
  // Trying to access the directory
  fs.accessSync(directoryPath);
  console.log('Directory exists!');
} catch (error) {
  // Handling the error if the directory does not exist
  console.log('Directory does not exist!');
}
```

Both methods provide the same result, but the `existsSync()` method is more straightforward and does not require error handling.

## Deep Dive

When using either of the above methods, it is important to note that the directory path must be relative to the current working directory. If we want to check for a directory in a different path, we need to use the `path` module to resolve the absolute path first.

Another important thing to keep in mind is that the check for directory existence does not guarantee that the directory is accessible or even a directory at all. It only verifies if the given path exists in the file system. Therefore, it is still possible for the directory to have limited or no permissions, or for the path to point to a file instead of a directory.

To handle these potential issues, we can use the `fs.stat()` method which provides more detailed information about the object referenced by the path, including its type and permissions. We can then use this information to further validate the existence and accessibility of the directory.

## See Also

- [Node.js fs Module Documentation](https://nodejs.org/api/fs.html)
- [TypeScript Handbook: Namespaces](https://www.typescriptlang.org/docs/handbook/namespaces.html)
- [Path Module | Node.js v14.15.0 Documentation](https://nodejs.org/api/path.html)