---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Checking if a Directory Exists in TypeScript

## What & Why?

Checking if a directory exists is a check made by the program to see whether a certain file folder (directory) exists on your computer's file system. Programmers do this to prevent errors when trying to access or manipulate directories that might not exist.

## How to:

Let's see how to do it in TypeScript. We'll use Node.js' built-in `fs` module to get the job done. Have a look at the following example:

```TypeScript
import * as fs from 'fs';

const directoryToCheck = './some_directory';

fs.access(directoryToCheck, fs.constants.F_OK, (err) => {
    if (err) {
        console.log(`Directory doesn't exist`);
    } else {
        console.log(`Directory exists`);
    }
});
```

After running this script, if the directory './some_directory' exists, you'll see 'Directory exists'; otherwise, 'Directory doesn't exist' in your console.

## Deep Dive

Historically, the need to check if directories exist dates back to the earliest times of file system operations. Realizing that blindly carrying out operations on non-existent targets could cause problems, developers incorporated existence checks into their routines.

Alternatively, you could use the `fs.existsSync` function, but be cautious about its synchronous nature, which can block the main thread:

```TypeScript
import * as fs from 'fs';

if(fs.existsSync(directoryToCheck)) {
    console.log('Directory exists');
} else {
    console.log('Directory does not exist');
}
```

Behind the scenes, the `fs.access` function checks permissions for the file or directory specified. The `F_OK` flag, in particular, checks the existence of the path.

## See Also

For more details and options about the `fs` module, check out the [Node.js documentation](https://nodejs.org/api/fs.html). If you want to dive even deeper, explore the [Node File System tutorial](https://www.w3schools.com/nodejs/nodejs_filesystem.asp) from W3Schools. For advanced users, consider taking a look at the [File System section](https://nodejs.dev/learn/the-nodejs-fs-module) on Node.js.dev.