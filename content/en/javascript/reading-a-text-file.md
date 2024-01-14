---
title:    "Javascript recipe: Reading a text file"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, there are always situations where we need to read data from a file. Whether it's user input, configuration settings, or data from an external source, being able to read from a text file is an essential skill for any programmer.

## How To

To start off, we need to first understand the basics of reading a text file in JavaScript. There are a few steps involved in this process, but once you get the hang of it, it will become second nature.

First, we need to open the text file using the `fs` module, which is short for "file system". This module provides us with methods to read from and write to files. We can use the `fs.readFile()` method to read the contents of a file. The code below shows an example of how to do this:

```javascript
const fs = require('fs'); // imports the 'fs' module
fs.readFile('example.txt', 'utf-8', (err, data) => {
  if (err) throw err; // checks for errors
  console.log(data); // prints the contents of the file
});
```

In the code above, we use the `readFile()` method, passing in the name of the file we want to read, the encoding (in this case, we specify UTF-8), and a callback function with two parameters - an error object (if there are any errors) and the data from the file. Within the callback function, we check for errors and then log the data to the console.

Now, let's say we want to read the file line by line instead of getting the entire contents at once. We can use the `readline` module for this. The code below shows how to read a file line by line:

```javascript
const readline = require('readline'); // imports the 'readline' module
const fs = require('fs'); // imports the 'fs' module
const readInterface = readline.createInterface({
  input: fs.createReadStream('example.txt'),
  console: false
});

readInterface.on('line', function(line) {
  console.log(line); // prints each line to the console
});
```

In this code, we use the `createInterface()` method from the `readline` module to create a readable stream from the file. Then, we use the `on` event listener to read each line from the file and print it to the console.

## Deep Dive

In the previous section, we covered the basics of reading a text file in JavaScript. However, there are some finer details and edge cases that are worth exploring.

For example, what if we want to read a large file that might not fit into memory? In this case, it is better to use the `createReadStream()` method from the `fs` module. This method creates a readable stream that will read the file in chunks, avoiding any memory overflow.

Another important aspect to consider is the encoding of the file. In our examples above, we used `utf-8` as the encoding. However, it's important to use the correct encoding for the specific file you are reading to avoid any unexpected characters or errors.

## See Also

- [fs module documentation](https://nodejs.org/api/fs.html)
- [readline module documentation](https://nodejs.org/api/readline.html)
- [Understanding Streams in Node.js](https://nodejs.dev/learn/nodejs-streams)