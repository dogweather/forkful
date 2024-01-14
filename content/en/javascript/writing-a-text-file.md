---
title:                "Javascript recipe: Writing a text file"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file may not seem like the most exciting or glamorous task in the world of programming, but it is a fundamental skill that can be incredibly useful in many different scenarios. Whether you're looking to store data for later use or simply want to practice your coding abilities, knowing how to write a text file is a valuable skill to have in your arsenal.

## How To

To begin, let's take a look at the basic structure of a text file. A text file consists of a sequence of characters organized in a certain way to represent information. In JavaScript, we can use the built-in `fs` module to create and write to a text file. Let's see how this works with a simple coding example:

```Javascript
//Require the fs module
const fs = require('fs');

//Create a new text file called "output.txt"
fs.writeFile('output.txt', 'This is a sample text.', (err) => {
  //If an error occurs, throw an error
  if (err) throw err;

  //If no error, log a success message
  console.log('Text file successfully created and written to!');
});
```

In the example above, we first require the `fs` module and then use the `writeFile` method to create a new text file called "output.txt" and write the string "This is a sample text." to it. We also provide a callback function to handle any errors and print a success message if the file is successfully written.

Now, let's see how we can read and print the contents of our newly created text file using the `readFile` method:

```Javascript
//Read contents of "output.txt" file
fs.readFile('output.txt', 'utf8', (err, data) => {
  //If an error occurs, throw an error
  if (err) throw err;

  //If no error, print contents of file
  console.log(data);
});
```

In the second code block, we use the `readFile` method to read the contents of our text file and print it to the console. We also specify the character encoding as `utf8` to ensure we get the expected output.

## Deep Dive

While the examples above cover the basics of writing a text file in JavaScript, there are a few other things to keep in mind when working with text files. Here are a few key points to remember:

- When writing to a text file, if the given file name already exists, it will be overwritten. To avoid this, we can use the `appendFile` method instead to add new content to the end of the file.
- We can also specify the character encoding when reading a text file using the `readFile` method. This allows us to properly handle files containing non-English characters or other special characters.
- When working with large files, it is important to use the `createWriteStream` and `createReadStream` methods to prevent memory issues.

## See Also
- [Node.js File System documentation](https://nodejs.org/api/fs.html)
- [Writing files with Node.js](https://stackabuse.com/writing-files-with-node-js/)
- [Reading and writing files in Node.js](https://www.digitalocean.com/community/tutorials/reading-and-writing-files-in-node-js)