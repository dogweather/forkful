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

## Why 
Checking if a directory exists is an important task in many programming scenarios. It allows us to handle errors and ensure that our code is functioning as expected, by verifying the existence of a specific directory before performing any operations on it.

## How To
Checking if a directory exists in TypeScript is a straightforward process. We can use the `fs` module from Node.js to access file system methods, including checking for directory existence. Here is a simple example:

```TypeScript
import fs from 'fs';

const directoryPath = './myDirectory';

// Check if the directory exists
fs.exists(directoryPath, (exists) => {
    if (exists) {
        console.log("The directory exists!");
    } else {
        console.log("The directory does not exist!");
    }
});
```

Running this code in a TypeScript environment will output either "The directory exists!" or "The directory does not exist!" depending on the existence of the specified directory. 

We can also use the `fs.existsSync()` method to achieve the same result synchronously, which may be useful in certain scenarios. Here is an example:

```TypeScript
import fs from 'fs';

const directoryPath = './myDirectory';

// Check if the directory exists synchronously
if (fs.existsSync(directoryPath)) {
    console.log("The directory exists!");
} else {
    console.log("The directory does not exist!");
}
```

## Deep Dive
Under the hood, the `fs.exists()` and `fs.existsSync()` methods use the `fs.stat()` or `fs.lstat()` methods to check for directory existence. These methods return a `Stats` object that contains information about the file or directory. Specifically, the `isDirectory()` method of the `Stats` object can be used to determine if the specified path is a directory or not. Here is an example:

```TypeScript
import fs from 'fs';

const directoryPath = './myDirectory';

// Get the stats of the specified path
fs.stat(directoryPath, (err, stats) => {
    if (err) {
        // Handle error
    } else {
        // Check if the path is a directory
        if (stats.isDirectory()) {
            console.log("The path is a directory!");
        } else {
            console.log("The path is not a directory!");
        }
    }
});
```

It is important to note that these methods only check for the existence of a directory, not its permissions or accessibility. Additional checks may need to be implemented for such cases.

## See Also
- [Node.js `fs` module documentation](https://nodejs.org/api/fs.html)
- [TypeScript documentation](https://www.typescriptlang.org/docs/home.html)