---
title:    "TypeScript recipe: Checking if a directory exists"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

When working with TypeScript, it's common to encounter situations where you need to check if a directory exists. This could be useful for various reasons, such as making sure a file path is valid before attempting to access it, or for creating new folders if they do not exist yet.

## How To

Checking if a directory exists in TypeScript is a simple task that can be accomplished using the built-in "fs" module. First, we will need to import the module into our code:

```typescript
import * as fs from 'fs';
```

Next, we can use the "existsSync" function from the "fs" module to check if a directory exists or not. This function takes in a file path as an argument and returns a boolean value indicating if the directory exists or not.

```typescript
if (fs.existsSync('/path/to/directory')) {
  console.log('Directory exists');
} else {
  console.log('Directory does not exist');
}
```

The code above will first check if the directory exists at the specified file path and then log the appropriate message to the console. Additionally, we can use this function in a more dynamic way by using a variable for the file path.

```typescript
const directory = '/path/to/directory';
if (fs.existsSync(directory)) {
  console.log(`Directory "${directory}" exists`);
} else {
  console.log(`Directory "${directory}" does not exist`);
}
```

By using a variable, we can easily check multiple directories without repeating the same code over and over.

## Deep Dive

Behind the scenes, the "existsSync" function uses different techniques depending on the operating system to determine if a directory exists. On Windows, it uses the "GetFileAttributes" API call, while on UNIX-based systems it uses the "stat" system call. This allows for efficient and accurate directory checking on any platform.

It's also worth noting that the "existsSync" function only checks if the directory exists at the specified file path and does not differentiate between directories and files. So if the file path given leads to a file instead of a directory, the function will still return 'true'. Keep this in mind when using this function in your code.

## See Also

- [Official TypeScript Documentation](https://www.typescriptlang.org/)
- [Node.js File System Module](https://nodejs.org/api/fs.html)
- [Checking if a File Exists in TypeScript](https://dev.to/julikamble/checking-if-a-file-exists-with-typescript-4jo4)