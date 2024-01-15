---
title:                "Reading a text file"
html_title:           "Javascript recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a text file and wondered how it could be parsed and used in your Javascript code? Look no further! This article will explain the importance of reading a text file and how it can benefit your programming.

## How To

To read a text file in Javascript, we will be using the File System module, which provides an API for interacting with the file system in a way modeled on standard POSIX functions.

First, we need to declare the File System module by requiring it in our code:

```
const fs = require('fs');
```

Next, we will use the `readFile()` method to read the contents of the text file. The `readFile()` method takes in two arguments - the path to the file and the encoding format. For example, if we have a text file named `sample.txt` in the same directory as our code, we can use the following code to read its contents:

```
fs.readFile('sample.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

In the above code, we are using the `utf8` encoding format, which is the most commonly used format for text files. The `readFile()` method will read the contents of the file and store it in the `data` variable. We can then log the contents of the file to the console.

Let's say our `sample.txt` file contains the following text:

```
Hello world!
This is a sample text file.
```

The output of the above code would be:

```
Hello world!
This is a sample text file.
```

## Deep Dive

Now that we know how to read a text file in Javascript, let's delve into some additional information.

### Synchronous vs Asynchronous

In the above example, we used the asynchronous version of the `readFile()` method, which takes a callback function as an argument. However, there is also a synchronous version of the method called `readFileSync()`. The main difference between the two is that the synchronous version blocks the execution until the file is read, while the asynchronous version does not block the execution and allows the code to continue running while the file is being read.

### Handling Errors

In the `readFile()` method, we used a callback function with an `err` parameter to handle any errors that may occur while reading the file. It is important to handle errors properly in order to avoid any unexpected crashes in our program.

### Additional Resources

There are other methods provided by the File System module for reading text files such as `read()`, `readv()`, and `readSync()`. You can also check out the official documentation for more information and examples on reading text files.

## See Also

- [Official Node.js File System Documentation](https://nodejs.org/api/fs.html)
- [Reading and Writing Files in Node.js](https://stackabuse.com/reading-and-writing-files-in-node-js/)
- [Reading and Writing Text Files using Node.js](https://www.geeksforgeeks.org/reading-and-writing-text-files-using-node-js/)