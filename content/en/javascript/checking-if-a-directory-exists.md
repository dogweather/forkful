---
title:                "Javascript recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 

As developers, we often need to check if a directory exists before performing certain actions. This could be useful for creating new directories, checking for file existence, and many other tasks. Instead of running into errors, let's explore how we can efficiently check for the existence of a directory using Javascript.

## How To 

To check if a directory exists in Javascript, we can use the `fs` (file system) module. First, we need to require the `fs` module in our code using `const fs = require('fs');`. Then, we can use the `fs.existsSync()` method to check if a directory exists. Let's look at an example:

```Javascript
const fs = require('fs');

// check if directory exists
if (fs.existsSync('/path/to/directory')) {
    console.log("Directory exists!");
} else {
    console.log("Directory does not exist.");
}
```

In this code block, we use the `fs.existsSync()` method to check for the existence of a directory at the specified path. If the directory does exist, the `if` block will be executed and we will see the message "Directory exists!" in the console. Otherwise, the `else` block will be executed and we will see the message "Directory does not exist.".

We can also use the `fs.statSync()` method to check for the existence of a directory. Let's take a look at how we can do that:

```Javascript
const fs = require('fs');

// check if directory exists
try {
    const stats = fs.statSync('/path/to/directory');
    if (stats.isDirectory()) {
        console.log("Directory exists!");
    } else {
        console.log("Directory does not exist.");
    }
} catch (err) {
    console.log("Error: " + err);
}
```

In this code block, we use the `fs.statSync()` method to retrieve information about the specified path. If the path does not exist, it will throw an error which we can catch using a `try...catch` block. If the path does exist, we can use the `stats.isDirectory()` method to check if it is a directory. If it is a directory, we will see the message "Directory exists!" in the console. Otherwise, we will see the message "Directory does not exist.".

## Deep Dive 

Behind the scenes, the `fs.existsSync()` and `fs.statSync()` methods use the `lstat` system call to check for file existence. This system call returns metadata about a file or directory in the form of a `Stats` object. The `Stats` object contains information such as the file size, permissions, timestamps, etc. 

When checking for directory existence, we use the `isDirectory()` method on the `Stats` object to determine if the path points to a directory. If it does, the method returns `true`, otherwise it returns `false`. This allows us to handle our code accordingly and avoid errors.

## See Also 

- Node.js `fs` Module: https://nodejs.org/api/fs.html
- `fs.existsSync()` Method: https://nodejs.org/api/fs.html#fs_fs_existssync_path
- `fs.statSync()` Method: https://nodejs.org/api/fs.html#fs_fs_statsync_path