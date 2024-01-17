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

## What & Why?
Reading a text file is the process of retrieving information from a file that contains written data. Programmers use this technique to access and manipulate data from external sources, such as user input or database information. It allows for dynamic and flexible data handling in applications and can be used for a variety of purposes, from data analysis to file management.

## How to:
To read a text file in Javascript, we can use the built-in ```readFile``` function from the ```fs``` module. First, we need to import the module using the ```require()``` method:

```Javascript
const fs = require('fs');
```

Once we have access to the ```fs``` module, we can make use of the ```readFile``` function to read the contents of a text file. The function takes in two arguments - the file path and a callback function to handle the data. Here's an example of how we can read and log the data from a text file:

```Javascript
fs.readFile('sample.txt', (err, data) => {
    if (err) {
        console.error(err);
    } else {
        console.log(data.toString());
    }
});
```

This code will log the contents of the ```sample.txt``` file to the console. We can also specify a specific encoding, such as ```utf8```, to ensure that the data is returned in a readable format.

## Deep Dive:
Reading and writing text files has been a fundamental aspect of programming since its early days. Before the implementation of filesystems, data was stored in sequential order in text files, making it easy to read and manipulate with code. However, with the introduction of databases and other data storage solutions, the need to read text files has become less prominent.

While Javascript's built-in ```fs``` module is the most common way to read text files, there are other methods such as using third-party libraries or using Node.js's ```readline``` module for reading large files line by line. Reading text files can also be done in other programming languages, with similar functions and methods available.

## See Also:
- [Node.js Documentation on File System](https://nodejs.org/api/fs.html)
- [Introduction to JavaScript File Handling](https://www.educative.io/edpresso/how-to-read-and-write-files-in-javascript)