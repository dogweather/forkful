---
title:    "Javascript recipe: Checking if a directory exists"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why
As a programmer, you may come across situations where you need to check if a specific directory exists. This can be useful for various tasks such as file manipulation or error handling. By checking for the existence of a directory, you can ensure that your code runs smoothly without any unexpected crashes.

## How To
Checking if a directory exists in JavaScript can be done using a simple ```fs.existsSync``` function. This function takes in the path to the directory as an argument and returns a boolean value indicating whether the directory exists or not. Let's take a look at an example:

```
const fs = require('fs');

// path to directory we want to check 
const dirPath = './my-directory';

// checking if directory exists 
if (fs.existsSync(dirPath)) {
  console.log('Directory exists!');
} else {
  console.log('Directory does not exist!');
}
```

In this example, we first require the built-in ```fs``` module to work with the file system. Then, we define the path to the directory we want to check and use the ```fs.existsSync``` function to check its existence. Finally, based on the return value of the function, we log the appropriate message to the console.

Output for the above code will be:

```
Directory exists!
```

In case the directory does not exist, the output will be:

```
Directory does not exist!
```

## Deep Dive
Now, let's dive deeper into the ```fs.existsSync``` function. This function is a part of the built-in ```fs``` module in Node.js and is used to check the existence of any file or directory. It returns a boolean value, ```true``` if the file/directory exists and ```false``` if it doesn't. This function will also return false if the path provided is a symbolic link that points to a non-existing location.

If you are using the latest versions of Node.js, it is recommended to use the ```fs.statSync``` function instead. This function returns an object with information about the file/directory, including its existence.

## See Also
- [Node.js official documentation for fs.existsSync](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Node.js official documentation for fs.statSync](https://nodejs.org/api/fs.html#fs_fs_statsync_path_options)