---
title:                "Javascript recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a common task for programmers, whether it's for processing data, importing information, or simply just reading the contents. It's a useful skill to have in your coding arsenal, and in this blog post, we'll explore how to read a text file using Javascript.

## How To

Reading a text file in Javascript is a fairly simple process. First, we need to open the file using the `fs` (file system) module. This module is built-in to Javascript and allows us to work with files on our system.

````Javascript
const fs = require('fs'); //Import the fs module

// Open the text file and read its contents
fs.readFile('example.txt', 'utf8', function(err, data) {
    if (err) throw err; // Throw an error if the file doesn't exist

    // Print the contents of the file
    console.log(data);
});
````

In the code above, we use the `fs.readFile()` method to open our text file, `example.txt`, in utf8 format. This method takes in three parameters: the file name, the encoding (in this case, utf8), and a callback function that is executed when the file is successfully opened. The callback function has two parameters: an error object and the data from the file. 

We use `console.log()` to print the data from the file to the console. If there is an error with opening the file, we use `throw` to stop the execution of our code and display the error message.

### Sample Output

If we have a text file with the following contents:

```
Hello world!
```

Running the code above would result in the following output in our console:

```
Hello world!
```

## Deep Dive

While the method shown above is a simple and effective way to read a text file, there are other options as well. The `fs` module also has a `readFileSync()` method which allows us to synchronously read a file. This means our code will wait for the file to be read before moving on to the next line.

````Javascript
const fs = require('fs'); //Import the fs module

// Open the text file and read its contents synchronously
let data = fs.readFileSync('example.txt', 'utf8');

// Print the contents of the file
console.log(data);
````

In this code, we use `fs.readFileSync()` to read our text file synchronously. We store the data from the file in a variable and then print it to the console using `console.log()`.

## See Also

- [Node.js fs Module Documentation](https://nodejs.org/api/fs.html)
- [Reading and Writing Files in Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)

Reading a text file in Javascript may seem like a simple task, but it's important to understand the different methods available and how they work. With this knowledge, you can confidently work with text files and incorporate them into your projects seamlessly. Happy coding!