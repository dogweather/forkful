---
title:    "Javascript recipe: Checking if a directory exists"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 

If you're a web developer or programmer, there may be times when you need to check if a directory exists. This is a useful task to perform as it allows you to validate the existence of a specific folder before attempting to access it.

## How To 

To check if a directory exists in Javascript, we can use the `fs` (file system) module. This module provides several methods for working with files and directories, including the `fs.access()` method. 

```Javascript 
const fs = require('fs'); 

// Specify the name of the directory we want to check 
const directory = 'myFolder'; 

// Use the fs.access() method to determine if the directory exists 
fs.access(directory, (err) => { 
  if (err) { 
    console.error(err); 
    return; 
  } 
  // If no error is returned, the directory exists 
  console.log('The directory exists.'); 
}); 
```

Running this code will result in the following output if the directory exists: 

``` 
The directory exists. 
```

And if the directory does not exist, the output will be an error. 

``` 
Error: ENOENT: no such file or directory, access 'myFolder' 
```

## Deep Dive 

The `fs.access()` method is used to check the accessibility of a file or directory. It takes three arguments: the file or directory path, an optional `mode` argument which specifies the accessibility mode, and a callback function. 

The `fs.access()` method checks the file or directory for the specified `mode` access. If the file or directory is accessible, the callback function is invoked with no error. If the file or directory is not found or is not accessible, an error will be passed to the callback function. 

You can also use the `fs.stat()` method to check the existence of a directory. This method takes in a file or directory path and returns a `Stats` object which contains information about the file or directory. If the object is not found, an error will be returned. 

```Javascript 
const fs = require('fs'); 

// Specify the name of the directory we want to check 
const directory = 'myFolder'; 

// Use the fs.stat() method to determine if the directory exists 
fs.stat(directory, (err, stats) => { 
  if (err) { 
    console.error(err); 
    return; 
  } 
  // If no error is returned, the directory exists 
  console.log('The directory exists.'); 
}); 
``` 

## See Also

- [Node.js File System Module Documentation](https://nodejs.org/api/fs.html)
- [Checking if a File Exists in Node.js](https://www.geeksforgeeks.org/node-js-fs-exists-method/)
- [The fs.stat() Method in Node.js](https://www.digitalocean.com/community/tutorials/nodejs-checking-if-a-file-directory-exists#using-the-fs.stat-method)