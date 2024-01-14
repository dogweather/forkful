---
title:                "Javascript recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing text files is an essential skill for any programmer, as it allows for easily storing and accessing data without the use of a database. Whether you need to save user preferences, log data, or simply store information for later use, writing a text file is a valuable tool in your programming arsenal.

## How To

To write a text file in Javascript, we can use the built-in `fs` module. We first require the module at the top of our code:

```Javascript
const fs = require('fs');
```

Next, we use the `fs.writeFile()` method to create a new file and write data to it. The method takes three parameters: the file name, the data to be written, and a callback function. The callback function will be executed once the file has been successfully written.

```Javascript
fs.writeFile('textFile.txt', 'Hello World!', (err) => {
    if (err) throw err;
    console.log('The file has been saved!');
});
```

In the above example, we are creating a new file named `textFile.txt` and writing the string "Hello World!" to it. If an error occurs, such as the file already exists, the callback function will throw an error. Otherwise, the console will log a success message.

To add data to an existing file, we can use the `fs.appendFile()` method instead, which takes the same parameters as `fs.writeFile()` but appends the data to the end of the file instead of overwriting it.

```Javascript
fs.appendFile('textFile.txt', ', have a great day!', (err) => {
    if (err) throw err;
    console.log('The data has been added to the file!');
});
```

## Deep Dive

Although writing text files in Javascript is a relatively straightforward task, there are a few things to keep in mind. 

First, when using the `fs.writeFile()` and `fs.appendFile()` methods, the file name must include the file extension (e.g. `.txt`, `.js`, etc). If not specified, the file extension will default to `.txt`.

Secondly, these methods will create a new file if one with the same name does not already exist. However, if a file with the same name does exist, it will be overwritten with the new data. To avoid accidentally overwriting data, it's crucial to check for existing files before writing to them.

Lastly, when writing and appending to files, we can use string interpolation to make our code more dynamic. For example:

```Javascript
let name = 'John';
fs.writeFile('textFile.txt', `Hello ${name}!`, (err) => {...});
```
In this example, we are using the value of the `name` variable to dynamically generate the data to write to the file.

## See Also
- [Node.js fs Module Documentation](https://nodejs.org/api/fs.html)
- [MDN Web Docs on Writing Files with Node.js](https://developer.mozilla.org/en-US/docs/Learn/Server-side/Node_server_without_framework/Writing_files)
- [Tutorial: Writing and Reading Files in Node.js](https://www.digitalocean.com/community/tutorials/nodejs-reading-and-writing-files)

By learning how to write text files in Javascript, you'll have a powerful tool at your disposal for handling data in your projects. So go ahead and give it a try in your next project!