---
title:                "Checking if a directory exists"
html_title:           "Javascript recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

So you want to check if a directory exists in your Javascript code? Well, there are a few reasons why you might want to do this. Maybe you want to make sure that a certain directory exists before trying to save a file to it, or perhaps you want to ensure that you are working with a valid directory before performing any operations on it. Whatever the reason may be, checking if a directory exists can help prevent errors and improve the overall functionality of your code. But how exactly do you go about doing this? Let's dive into the details.

## How To

Checking if a directory exists in Javascript is a fairly straightforward process. You will need to use the `fs` module, which is part of the standard library in Node.js. First, you will need to import the `fs` module at the top of your file:

```Javascript
const fs = require('fs');
```

Next, you will need to use the `fs.existsSync()` method, passing in the path to the directory you want to check as an argument. This method will return a boolean value indicating whether or not the directory exists. Here's an example:

```Javascript
const fs = require('fs');

const directoryPath = './example';

if (fs.existsSync(directoryPath)) {
  console.log('The directory exists!');
} else {
  console.log('The directory does not exist.');
}
```

If the directory exists, the output will be `The directory exists!` If the directory does not exist, the output will be `The directory does not exist.`

## Deep Dive

Now, let's take a closer look at the `fs.existsSync()` method. This method checks if a path exists and, by default, it will check for both files and directories. If you only want to check if a directory exists, you can specify the `fs.constants.F_OK` flag as a second argument:

```Javascript
if (fs.existsSync(directoryPath, fs.constants.F_OK)) {
  // code for if directory exists
} else {
  // code for if directory does not exist
}
```

You can also use the `fs.existsSync()` method to check for the existence of a file by specifying the `fs.constants.R_OK` flag. Additionally, you can use the `fs.statSync()` method to check for more specific details about the path, such as whether it is a file or a directory, when it was last modified, and its size.

## See Also

- [Node.js fs module documentation](https://nodejs.org/api/fs.html)
- [How to check if a file or directory exists in Node.js](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-node-js)