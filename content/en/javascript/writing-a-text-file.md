---
title:                "Writing a text file"
html_title:           "Javascript recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file using Javascript might seem like a simple task, but it can actually serve a variety of purposes. Whether you're creating a log file for debugging or storing data for your application, knowing how to write a text file can be a useful skill for any Javascript developer.

## How To
To write a text file in Javascript, we will be using the Node.js fs (file system) module. This module allows us to perform file-related operations, such as creating, reading, and writing files, using functions with simple syntax. With that in mind, let's take a look at how to write a text file step by step.

First, we need to require the fs module in our code:

```
const fs = require('fs');
```

Next, we will use the `writeFile()` function provided by the fs module to write our text file. This function takes three arguments: the file name, the data to be written, and a callback function. The callback function will be executed after the file has been written.

```
fs.writeFile('example.txt', 'This is a sample text file.', (error) => {
  if(error) {
    // handle error if file cannot be written
  }
  else {
    // file has been written successfully
  }
});
```

Voila! With just a few lines of code, we have successfully written a text file named "example.txt" with the content "This is a sample text file.".

But what if we want to add more text to our file instead of overwriting it every time? In that case, we can use the `appendFile()` function instead. This function works in a similar way to `writeFile()`, but it will append the specified data to the end of the file instead of overwriting it.

```
fs.appendFile('example.txt', '\nThis is another line.', (error) => {
  if(error) {
    // handle error if file cannot be written
  }
  else {
    // more text has been added to the file successfully
  }
});
```

And that's all there is to it! You can now write text files in Javascript using the fs module.

## Deep Dive
Behind the scenes, the fs module uses system calls to perform file operations, making it a fast and efficient way to write files in Javascript. It also allows us to specify the encoding of the file, allowing us to write in different languages and formats.

Keep in mind that the file paths used in the `writeFile()` and `appendFile()` functions are relative to the location of the current script file. This means that if you want to create the file in a specific directory, you will need to specify the full path in the `fileName` argument.

## See Also
For more information on the Node.js fs module, check out the official documentation [here](https://nodejs.org/api/fs.html). You can also explore other file manipulation options in Javascript using libraries such as [fs-extra](https://www.npmjs.com/package/fs-extra) and [path](https://www.npmjs.com/package/path). Happy coding!