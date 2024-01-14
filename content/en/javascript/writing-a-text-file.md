---
title:    "Javascript recipe: Writing a text file"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why
Writing a text file is an essential skill for every aspiring programmer. It allows you to store and manipulate data in a structured way, making it easier to process and analyze information. Whether you're a beginner or an experienced developer, understanding how to write a text file is a crucial aspect of programming.

## How To
To write a text file in Javascript, you can use the built-in File System module. First, we need to require the module by using the 'require' keyword and assigning it to a variable like so:

```Javascript
const fs = require('fs');
```

Next, we can use the `fs.writeFile()` method to write a text file. This method takes in three parameters - the file name, the data to be written, and a callback function. Let's say we want to create a file called 'myFile.txt' with the text 'Hello World' written in it. We can do so using the following code:

```Javascript
fs.writeFile('myFile.txt', 'Hello World', (err) => {
  if (err) throw err;
  console.log('File created successfully!');
});
```

If the file does not already exist, this code will create it. If it does exist, the content will be overwritten. Additionally, we can use the `fs.appendFile()` method to add content to an existing file without overwriting its previous content. 

## Deep Dive
When writing a text file, it's important to understand the concept of encoding. Encoding refers to the process of converting characters into a specific format for storage or transmission. By default, the `fs.writeFile()` and `fs.appendFile()` methods use the 'utf-8' encoding, which supports most characters and is widely used.

If you want to use a different encoding, you can specify it as a parameter in the write methods. For example, if you want to use the 'ascii' encoding, you can do so like this:

```Javascript
fs.writeFile('myFile.txt', 'Hello World', 'ascii', (err) => {
  if (err) throw err;
  console.log('File created successfully with ascii encoding!');
});
```

You can find a list of supported encodings in the [NodeJS documentation](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback).

## See Also
- [NodeJS Documentation on File System](https://nodejs.org/api/fs.html)
- [W3Schools Tutorial on Writing Files with NodeJS](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [MDN Web Docs on File Encoding](https://developer.mozilla.org/en-US/docs/Glossary/Encoding)

By understanding the fundamentals of writing a text file in Javascript, you can now easily store and manipulate data in your programs. Make sure to explore different encoding options and try out some coding examples to solidify your understanding. Happy coding!