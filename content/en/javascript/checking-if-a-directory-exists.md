---
title:                "Checking if a directory exists"
html_title:           "Javascript recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a common task in programming, especially when dealing with file systems. It allows us to determine if a certain directory is present or not, which is important when developing applications that require specific folders to be present in the system. We do this to ensure that our program can function properly and handle any potential errors that may arise if the directory is missing.

## How to:

To check if a directory exists in Javascript, we can use the `fs` module which comes built-in with Node.js. We can use the `existsSync()` method to check if a directory exists, and it returns a boolean value indicating its presence. Here's an example of how it looks in code:

```Javascript
const fs = require('fs');

if (fs.existsSync('/path/to/directory')) {
  console.log("Directory exists!");
} else {
  console.log("Directory does not exist :(");
}
```

Output when directory exists:

```
Directory exists!
```

Output when directory does not exist:

```
Directory does not exist :(
```

## Deep Dive:

### Historical Context:

Before the `fs` module was introduced in Node.js, developers had to rely on external libraries or native commands to check if a directory exists. This often required writing more code and using external dependencies, making the process more cumbersome and less efficient.

### Alternatives:

Apart from using the `fs` module, developers can also use the `fs.access()` method to check if a directory exists. It is a more versatile method as it allows us to check for the existence of files and directories, and also provides more detailed information in case of failures. However, for simply checking if a directory exists, the `existsSync()` method is a more straightforward and efficient approach.

### Implementation Details:

The `fs.existsSync()` method performs a synchronous operation, meaning it blocks the main thread while it checks for the existence of a directory. This can cause performance issues in larger applications as it halts the execution until the operation is completed. To avoid this, we can use the asynchronous version, `fs.exists()`, which takes a callback function and does not block the main thread.

## See Also:

- [Node.js Official Documentation for fs Module](https://nodejs.org/api/fs.html)
- [fs-exists-cached Github Repository](https://github.com/isaacs/fs-exists-cached)