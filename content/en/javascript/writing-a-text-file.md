---
title:                "Writing a text file"
html_title:           "Javascript recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in Javascript means creating a file on our computer that contains text-based data. Programmers often do this when they want to store data in a file that can be easily readable or shared with others. It allows for the storage and retrieval of important information, making it a useful tool for developers.

## How to:

To write a text file in Javascript, we can use the `fs` module from Node.js. We first need to import it into our code using the `require()` function. Then, we can use the `writeFile()` method to create a new file and write data to it.

```
const fs = require('fs');

fs.writeFile('myFile.txt', 'Hello world!', function(err) {
  if (err) throw err;
  console.log('File created and data written successfully!');
});
```

This code will create a new file named `myFile.txt` in the current directory with the contents "Hello world!". If the file already exists, it will be overwritten with the new data.

## Deep Dive:

### Historical Context:

Text files have been used for storing and sharing data since the beginning of computing. However, in the early days of programming, saving data to a file using low-level languages like C or Assembly could be a complex and error-prone process. With the rise of high-level languages like Javascript, writing text files has become much simpler and more accessible for developers.

### Alternatives:

While writing a text file using the `fs` module is the most common and straightforward method, there are other options available. Many popular libraries, such as `fs-extra`, provide additional features and functionality for reading and writing files. Another alternative is to use a database like MongoDB to store and retrieve data instead of creating a text file.

### Implementation Details:

The `writeFile()` method takes three parameters: the file name, the data to be written, and a callback function. The callback function is executed once the file has been successfully created or an error has occurred. If the file already exists, it will be overwritten unless we specify a `flag` parameter with the value `a` for appending data.

## See Also:

- [Node.js fs Documentation](https://nodejs.org/api/fs.html)
- [fs-extra library](https://www.npmjs.com/package/fs-extra)
- [MongoDB](https://www.mongodb.com/)