---
title:                "Writing a text file"
html_title:           "TypeScript recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file in TypeScript can be a useful tool for organizing and storing data, creating documentation, or simply for personal note-taking. It allows for structured and easily readable text that can be accessed and manipulated in various ways.

## How To

To write a text file in TypeScript, follow these steps:

1. Import the `fs` module from Node.js, which provides file system operations.
2. Use the `fs.writeFile()` method to write data to a file, providing the file name, data, and a callback function to handle any errors.
3. Within the callback function, use the `if(err)` statement to check for errors and log them if they occur.
4. If there are no errors, log a success message.
5. Use the `fs.appendFile()` method to add additional data to the file, if desired.
6. To read the file, use the `fs.readFile()` method and pass in the file name and encoding type (usually `utf-8`).
7. Within the callback function, use the data parameter to access the contents of the file.

Here is a code example of writing and reading a text file:

```TypeScript
import * as fs from 'fs';

// Writing to a file
fs.writeFile('myFile.txt', 'This is some example text.', (err) => {
    // Error handling
    if (err) {
        console.log(err);
    } else {
        console.log('File successfully written.');
    }
});

// Appending to a file
fs.appendFile('myFile.txt', '\nHere is some more text.', (err) => {
    // Error handling
    if (err) {
        console.log(err);
    }
});

// Reading the file
fs.readFile('myFile.txt', 'utf-8', (err, data) => {
    // Error handling
    if (err) {
        console.log(err);
    } else {
        console.log(data); // Outputs: "This is some example text. \nHere is some more text."
    }
});
```

## Deep Dive

In addition to the basic functionality of writing and reading a text file, there are various options and methods available to further customize and manipulate the data. These include specifying the file encoding, using the `fs.rename()` method to rename a file, or using the `fs.unlink()` method to delete a file. The `fs.createReadStream()` and `fs.createWriteStream()` methods also provide more control over reading and writing large or binary files.

## See Also

- [Node.js `fs` Module Documentation](https://nodejs.org/api/fs.html)
- [Node.js `fs` Module Tutorial](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)