---
title:                "Checking if a directory exists"
html_title:           "TypeScript recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a common programming task where the code checks if a specified directory exists on the operating system's file system. This is important for programmers so that they can handle cases where the directory does not exist, preventing potential errors in their code.

## How to:

To check if a directory exists in TypeScript, we can use the built-in `fs` (file system) module. First, we need to import the module with `import fs from "fs";`. Then, we can use the `fs.existsSync()` method, passing in the path to the directory we want to check. This method will return a boolean value, `true` if the directory exists and `false` if it does not.

```TypeScript
import fs from "fs";

const directoryPath = "path/to/directory";
const directoryExists = fs.existsSync(directoryPath);
console.log(directoryExists); // outputs: true or false
```

## Deep Dive:

The `fs.existsSync()` method is available in all versions of Node.js, but it is considered a legacy API. The recommended way of handling file system operations in newer versions of Node.js is to use the `fs.statSync()` method, passing in the path to the directory and checking the `fs.Stats` object it returns.

```TypeScript
import fs from "fs";

const directoryPath = "path/to/directory";

try {
  const stats = fs.statSync(directoryPath);
  console.log(stats.isDirectory()); // outputs: true
} catch (error) {
  console.log(error); // outputs: "Error: ENOENT: no such file or directory"
}
```

In addition to the `fs` module, there are other libraries available for file system operations, such as `path` and `graceful-fs`. These libraries provide more advanced features and better error handling when working with directories and files.

## See Also:

- [Node.js `fs` module documentation](https://nodejs.org/api/fs.html)
- [`path` library documentation](https://nodejs.org/api/path.html)
- [`graceful-fs` library documentation](https://github.com/isaacs/node-graceful-fs)