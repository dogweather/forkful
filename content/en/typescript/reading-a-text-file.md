---
title:                "Reading a text file"
html_title:           "TypeScript recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file is the process of accessing and extracting information from a text-based document using a computer program. Programmers often do this to automate tasks such as data analysis, parsing, or creating reports from large amounts of data contained within a text file.

## How to:
To read a text file in TypeScript, we can use the `fs` module from Node.js. First, we need to import the module by adding this line at the top of our code: 
```
import { readFileSync } from 'fs';
```
Next, we need to specify the path of the text file we want to read. For example, if our text file is located in the same folder as our TypeScript file, we can use the following code:
```
const filePath = './myTextFile.txt';
```
Then, we can use the `readFileSync()` method to read the contents of the file and store it in a variable, like this:
```
const fileContents = readFileSync(filePath, 'utf-8');
```
The `utf-8` parameter specifies the character encoding of the file. Finally, we can output the contents of the file by logging the variable to the console:
```
console.log(fileContents);
```

## Deep Dive:
In the past, reading a text file in TypeScript was not as straightforward as it is now. Before the `fs` module was introduced, developers had to rely on external libraries or use `XMLHttpRequest` to access the file. However, the `fs` module provides a more efficient and convenient way to read files, especially when dealing with large amounts of data.

An alternative to using the `fs` module is using the `readFile()` method, which allows for asynchronous file reading. This can be useful when we want to read multiple files at the same time, without blocking other operations. However, it requires the use of callbacks or promises, making the code more complex.

When reading a text file, it is important to consider the character encoding of the file. The `utf-8` encoding is widely used and can handle most languages, but other encodings may be needed for specific languages or characters.

## See Also:
- [Node.js `fs` module documentation](https://nodejs.org/api/fs.html)
- [Understanding character encoding](https://www.w3.org/International/questions/qa-what-is-encoding)
- [A comparison of `readFileSync()` and `readFile()`](https://stackoverflow.com/questions/17604866/difference-between-fs-readfilesync-vs-fs-readfile)