---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in programming refers to accessing the contents of a file on your disk and loading it into your program. It serves a basic but crucial function in data manipulation—allowing developers to load, analyze, and manipulate external data.

## How To:

Reading a text file in JavaScript is straightforward. We'll use the File System (`fs`) module that ships with Node.js. Here's a simple example:

```Javascript
var fs = require('fs');

fs.readFile('yourFile.txt', 'utf8', function(err, data) {
    if (err) throw err;
    console.log(data);
});
```

This script reads the contents of `yourFile.txt` and logs them to the console. Make sure to replace `'yourFile.txt'` with the path to the file you'd like to read.

## Deep Dive

Reading text files has been a part of programming since its earliest days, where punch cards were read into machines line by line. Today we're working with much more complex and larger datasets, but the basic concept remains.

As for alternatives, there's the `fs.readFileSync()` method, which works similarly but operates synchronously, blocking the event loop until file reading completes. Avoid using this in performance-critical applications, but it could be fine for simpler scripts or capacity-light tasks. 

The implemented `readFile` method uses a buffered read strategy to conserve memory usage. It lowers memory demand, thus increasing the efficiency and speed of your program.

## See Also:

Investigate these sources for further reading:

- [JavaScript File API](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)
- [Async/Await Introduction](https://javascript.info/async-await)
- [JavaScript Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises)