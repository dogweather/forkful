---
title:    "TypeScript recipe: Creating a temporary file"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
When writing programs or scripts, temporary files can be useful for storing data or information that is only needed for a short amount of time. These files can also be used for caching or as a way to quickly save and retrieve data.

## How To
Creating a temporary file in TypeScript is a simple process. First, we need to import the `fs` (file system) module:
```TypeScript
import fs from 'fs';
```
Next, we can use the `fs` module's `mkdtempSync()` function to create a temporary directory and return its path:
```TypeScript
const tempDir = fs.mkdtempSync('/tmp/');
```
We can then use the `fs` module's `writeFileSync()` function to write data to our temporary file, specifying the filename as well as the data to be written:
```TypeScript
fs.writeFileSync(`${tempDir}/tempfile.txt`, 'This is a temporary file!');
```
To retrieve the data from our temporary file, we can use the `fs` module's `readFileSync()` function:
```TypeScript
const data = fs.readFileSync(`${tempDir}/tempfile.txt`, 'utf8');
console.log(data);
// Output: 'This is a temporary file!'
```
Lastly, we can delete the temporary file by using the `fs` module's `unlinkSync()` function:
```TypeScript
fs.unlinkSync(`${tempDir}/tempfile.txt`);
```
And that's it! We have successfully created, written to, read from, and deleted a temporary file in our TypeScript code.

## Deep Dive
Under the hood, the `fs` module's `mkdtempSync()` function uses the operating system's default temporary directory to create the temporary file. This can usually be found at `/tmp` on UNIX systems and `C:\Windows\Temp` on Windows systems.

By default, the created temporary directory will have a random directory name prefix. This is to ensure unique paths and prevent any potential conflicts with existing files or directories. However, we can also specify our own prefix by passing it in as the first argument to the `mkdtempSync()` function.

In addition to the `mkdtempSync()` function, the `fs` module also provides `mkdtemp()` which is an asynchronous version of the function and allows for more flexibility in handling errors and callbacks.

## See Also
- [Node.js `fs` Module Documentation](https://nodejs.org/api/fs.html)
- [Creating and Deleting Temporary Files in TypeScript](https://www.cloudfuze.com/creating-deleting-temporar