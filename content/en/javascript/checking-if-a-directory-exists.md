---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in your JavaScript code simply means verifying if a given directory path is available, or in developer slang, "exists". This helps prevent errors when we attempt to read, write, or modify files in a non-existent directory.

## How to:

Here's a simple way to do this in Node.js using the fs module's `existsSync()` method:

```Javascript
const fs = require('fs');

if (fs.existsSync('/path/to/the/directory')) {
    console.log('Directory exists!');
} else {
    console.log('Directory not found');
}
```
This code will return either 'Directory exists!' or 'Directory not found' depending on whether the directory exists.

## Deep Dive

Historically, `fs.exists()` was used but has been deprecated since it used a non-standard callback argument ordering and was inconsistent with other Node.js callbacks. The currently recommended method is `fs.existsSync()`.

As an alternative, the `fs.access()` method can be used. It's argued to be more accurate as it checks for user permissions, not just existence. Here's how you do it:

```Javascript
const fs = require('fs');

fs.access('/path/to/the/directory', (error) => {
    if (error) {
        console.log('Directory not found');
    } else {
        console.log('Directory exists!');
    }
});
```

Remember that these methods only work server-side with Node.js. For client-side JavaScript that runs in the browser, this kind of file system access is restricted due to security reasons.

## See Also

For more in-depth info: 

1. Node.js documentation on the `fs` module: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html).
2. Blog post discussing `fs.access()` vs `fs.existsSync()`: [https://ar.al/2015/10/03/synchronous-and-asynchronous-error-handling-in-node.js/](https://ar.al/2015/10/03/synchronous-and-asynchronous-error-handling-in-node.js/).
3. StackOverflow post about client-side JavaScript limitations: [https://stackoverflow.com/questions/183214/javascript-node-js-file-system-operations-are-limited](https://stackoverflow.com/questions/183214/javascript-node-js-file-system-operations-are-limited).