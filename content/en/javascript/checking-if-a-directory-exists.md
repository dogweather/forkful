---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:56:53.960070-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is about confirming whether a folder is present at a specified path in the file system. Programmers do it to prevent errors like trying to read from or write to a directory that isn’t there.

## How to:
In JavaScript (running in a Node.js environment), there's a built-in module called `fs` that you can use to check if a directory exists. Here's a quick example:

```javascript
const fs = require('fs');
const path = './path/to/directory';

fs.access(path, fs.constants.F_OK, (err) => {
    if (err) {
        console.error(`${path} does not exist`);
    } else {
        console.log(`${path} exists`);
    }
});
```

Sample Output:

```
./path/to/directory exists
```

Or using the newer `fs.promises` API with async/await:

```javascript
const fs = require('fs').promises;

async function checkDirectoryExists(path) {
    try {
        await fs.access(path, fs.constants.F_OK);
        console.log(`${path} exists`);
    } catch {
        console.error(`${path} does not exist`);
    }
}

checkDirectoryExists('./path/to/directory');
```

Sample Output:

```
./path/to/directory does not exist
```

## Deep Dive
Historically, checking for a file or directory involved `fs.stat` or `fs.existsSync`, but these have drawbacks. `fs.stat` requires extra logic to determine if the path is a directory, and `fs.existsSync` is synchronous, which can block the event loop in Node.js.

An alternative is to use the `fs.promises` API or async/await for better readability and to keep your program non-blocking.

One implementation detail is that `fs.access` only checks for the existence, not the readability or writability of the directory. Other flags can be used with `fs.access` to check for those permissions if needed.

## See Also
- Node.js `fs` documentation: [Node.js fs module](https://nodejs.org/api/fs.html)
- More on async/await: [Async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- Info on file system flags: [File System Flags](https://nodejs.org/api/fs.html#file-system-flags)
