---
title:    "TypeScript recipe: Checking if a directory exists"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 
As developers, there may come a time when we need to check if a directory exists within our code. This could be for various reasons, such as verifying if a specific folder exists before creating a new one, or handling file operations related to that directory. Whatever the reason may be, understanding how to check for the existence of a directory in TypeScript is a valuable skill to have.

## How To 
To check if a directory exists in TypeScript, we can use the built-in `fs` module. This module provides a wide range of functionalities related to file operations. Specifically, we will be using the `existsSync()` method to check for the existence of a directory synchronously. This method takes in the path to the directory as an argument and returns a `boolean` value indicating whether the directory exists or not.

```TypeScript 
import * as fs from 'fs';

// specify the path to the directory we want to check
const directoryPath = './myDirectory';

// check if the directory exists using the fs.existsSync() method
const directoryExists: boolean = fs.existsSync(directoryPath);

if (directoryExists) {
  console.log('Directory exists!'); // output: Directory exists!
} else {
  console.log('Directory does not exist!'); // output: Directory does not exist!
}
```

In the above example, we have used the `existsSync()` method to check if the `myDirectory` folder exists in our project. If the directory does exist, the code will log `Directory exists!` to the console. Otherwise, it will log `Directory does not exist!`. This allows us to handle different scenarios based on the existence of the target directory.

## Deep Dive 
Under the hood, the `existsSync()` method makes use of the `fs.accessSync()` method which checks for the accessibility of a given file or directory. However, unlike `existsSync()`, the `accessSync()` method throws an error if the file or directory does not exist. To handle this, we can wrap the `accessSync()` method in a `try/catch` block to catch the error and return `false` instead of throwing it.

Additionally, it's important to note that the `existsSync()` method performs a synchronous operation, meaning it will block the execution of the code until the directory is checked. This can impact the performance of our application, especially if we have to check for multiple directories. In such cases, it's recommended to use the asynchronous version of the method, `exists()`, to avoid blocking the execution of the code.

## See Also 
- Official `fs` module documentation: https://nodejs.org/api/fs.html
- MDN web Docs for `existsSync()` method: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/fs/existsSync
- Blog post on asynchronous directory existence check in TypeScript: https://blog.logrocket.com/check-if-a-directory-exists-in-typescript/