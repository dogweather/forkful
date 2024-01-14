---
title:                "Javascript recipe: Checking if a directory exists"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to check if a certain directory exists before performing a specific task in your code? Maybe you need to make sure a directory is created before writing a file to it, or you want to make sure a certain folder is present before attempting to access its contents. In this blog post, we will discuss the importance of checking if a directory exists and how to do it in Javascript.

## How To

To check if a directory exists in Javascript, we can use the `fs` (file system) module which provides an `existsSync()` method. This method takes in the path of the directory as its parameter and returns a boolean value indicating if the directory exists or not. Let's see an example of how to use this method:

```Javascript
const fs = require('fs');

const directory = './myFolder';

if(fs.existsSync(directory)) {
    console.log("The directory exists!");
} else {
    console.log("The directory does not exist.");
}
```

In the above code, we first require the `fs` module and store the path of the directory we want to check in a variable. Then, we use the `existsSync()` method to check if the directory exists. If it does, we log a message stating that it exists, and if not, we log a message stating that it does not exist.

We can also use the `existsSync()` method to check if a file exists. We just need to pass in the path of the file instead of the directory. In addition, the `fs` module also provides an `exists()` method which works asynchronously and takes in a callback function as its second parameter.

## Deep Dive

When using the `existsSync()` method, it is important to note that the path we provide must be a valid path to a directory or a file. If the path is invalid or the user does not have permission to access the directory, the method will throw an error.

Another thing to keep in mind is that the `exists()` method uses a stat call to check if the directory or file exists. This means that it may return a false positive if the path is a dangling symbolic link.

## See Also

- [fs.existsSync() documentation](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [fs.exists() documentation](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)