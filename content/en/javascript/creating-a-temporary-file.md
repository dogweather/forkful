---
title:                "Javascript recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common task in programming, especially when dealing with large amounts of data or complicated operations. Temporary files can serve as a placeholder for data that needs to be processed or stored temporarily, or as a way to free up memory by storing data on disk rather than in memory.

## How To

To create a temporary file in Javascript, we can use the built-in `fs` module's `mkdtempSync()` method. This method takes in two parameters: a prefix for the temporary file name and a callback function. Here's an example of how we can use this method to create a temporary file and log its name to the console:

```javascript
const fs = require('fs');

// prefix for temporary file name
const prefix = 'myTempFile';

// creating temporary file and getting its name
const tempFile = fs.mkdtempSync(prefix, (err, folder) => {
  if (err) throw err;
  console.log('Temporary file created in: ' + folder);
});
```

The output of this code would be the name of the temporary file, something like `myTempFileJ7p4aG`. We can then use this file to store data or perform any other necessary operations.

## Deep Dive

Behind the scenes, the `mkdtempSync()` method uses Unix's `mkdtemp` function to create the temporary file. This function generates a unique name for the temporary file by appending a random string of characters to the provided prefix. These randomly generated names are typically based on timestamps, process IDs, and other system-specific information to ensure that each temporary file has a unique name.

It's important to note that temporary files are not automatically deleted. We must explicitly delete them using the `unlink()` method in the `fs` module. It's best practice to always delete temporary files after we no longer need them.

## See Also

Here are some resources for further learning and understanding:

- [Node.js Documentation on `fs` module](https://nodejs.org/api/fs.html)
- [GeeksforGeeks article on temporary files in Node.js](https://www.geeksforgeeks.org/create-a-temporary-file-using-node-js/)
- [Stack Overflow thread on deleting temporary files in Node.js](https://stackoverflow.com/questions/25348736/deleting-a-temporary-file-in-node-js)

Happy coding!