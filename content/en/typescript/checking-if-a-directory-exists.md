---
title:                "TypeScript recipe: Checking if a directory exists"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When writing a TypeScript program that needs to access files, it's important to first check if a directory exists before attempting to read or write to it. This is necessary to avoid errors and ensure that the program runs smoothly.

## How To

```typescript
// Import the fs module
import fs from 'fs';

// Define the directory path
const directoryPath = 'path/to/directory';

// Use fs.existsSync() to check if the directory exists
if (fs.existsSync(directoryPath)) {
  console.log('Directory exists!');
} else {
  console.log('Directory does not exist!');
}
```

The code above shows a simple example of using the `fs.existsSync()` method to check if a directory exists. The method takes in a path as its argument and returns a boolean value indicating whether the directory exists or not. 

It's important to note that the `fs.existsSync()` method does not differentiate between a file and a directory, so it's possible to get a `true` response even if a file exists at the specified path. To check specifically for a directory, we can use the `fs.lstatSync()` method and check for the "isDirectory()" property.

```typescript
if (fs.lstatSync(directoryPath).isDirectory()) {
  console.log('Directory exists!');
} else {
  console.log('Directory does not exist!');
}
```

## Deep Dive

Behind the scenes, the `fs.existsSync()` method uses the `fs.lstatSync()` method to gather information about the given path. `fs.lstatSync()` returns an fs.Stats object that contains information about the path, including whether it is a file or directory.

By using `fs.lstatSync()` instead of `fs.existsSync()`, we have access to more information about the path which can be helpful in certain situations. For example, we can use the `fs.Stats` object to check the size, ownership, and permissions of the underlying file or directory.

## See Also

* [Node.js Documentation on fs module](https://nodejs.org/api/fs.html)
* [TypeScript Documentation on fs module](https://www.typescriptlang.org/docs/handbook/nodejs.html#working-with-files)
* [Tutorial on checking if a directory exists in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-verify-if-a-file-or-directory-exists-in-a-node-js-application)