---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creating a Temporary File in JavaScript

## What & Why?

A temporary file is a type of file that stores data you need to use among different program instances, especially massive data sets. It's particularly useful when you want to share data between different parts of your code without using up too much system memory.

## How to:

Here's a basic Javascript way to create, write, and read a temporary file using the built-in `fs` (filesystem) module.

```Javascript
const fs = require('fs');
const os = require('os');
const path = require('path');

// Create a temporary file:
let tmpFile = path.join(os.tmpdir(), 'myTempFile.txt');
fs.writeFileSync(tmpFile, 'Hello, Temporary World!');

// Read from the temporary file:
let data = fs.readFileSync(tmpFile, 'utf8');
console.log(data);
```

If you run this, you'd see "Hello, Temporary World!" written to the console.

## Deep Dive

Historically, Unix systems created the concept of temporary files. These days, every major programming language has a way to implement temporary files, and JavaScript is no different.

Creating a temporary file is not the only way to handle large amounts of data in JavaScript. Arrays, database systems, or in-memory data structures like Redis could be alternatives, depending on your requirements.

When you create a file with the fs module in Node.js, it is created in your OS's default temp directory. The OS is responsible for cleaning these up eventually, but it's a good practice to delete any temporary files that you create once you're done with them.

## See Also 

- The Node.js fs documentation: https://nodejs.org/api/fs.html
- In-memory data store Redis: https://redis.io/
- JavaScript and Redis tutorial: https://www.sitepoint.com/using-redis-node-js