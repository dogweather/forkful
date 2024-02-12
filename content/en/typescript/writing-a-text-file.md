---
title:                "Writing a text file"
aliases:
- en/typescript/writing-a-text-file.md
date:                  2024-02-03T19:03:17.826760-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing a text file"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file in TypeScript is a critical skill for data persistence, configurations, or log generation. Programmers often perform this task to store and manipulate data outside the application memory for reasons like data analysis, reporting, or simply saving user settings across sessions.

## How to:
TypeScript itself doesn't directly handle file operations as it compiles to JavaScript, which traditionally runs in the browser with limited access to the file system. However, when used in a Node.js environment, the `fs` module (File System) provides functionality to write files.

### Using Node.js fs module
First, ensure you're working in a Node.js environment. Then, use the `fs` module to write text files. Here's a basic example:

```typescript
import * as fs from 'fs';

const data = 'Hello, world!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('The file has been saved!');
});
```

This will asynchronously write "Hello, world!" to `message.txt`. If the file does not exist, Node.js creates it; if it does, Node.js overwrites it.

For synchronous file writing, use `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'Hello again, world!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('The file has been saved!');
} catch (err) {
    console.error(err);
}
```

### Using popular third-party libraries
While the native `fs`module is powerful, some developers prefer using third-party libraries for additional convenience and functionality. `fs-extra` is a popular choice that extends `fs` and makes file operations more straightforward.

First, you'll need to install `fs-extra`:

```
npm install fs-extra
```

Then, you can use it in your TypeScript file to write text content:

```typescript
import * as fs from 'fs-extra';

const data = 'This is fs-extra!';
const filePath = './extraMessage.txt';

// Using async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('The file has been saved with fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

This code snippet does the same thing as the earlier `fs` examples but utilizes the `fs-extra` library, offering a cleaner syntax for handling promises.
