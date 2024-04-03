---
date: 2024-02-03 19:03:12.601106-07:00
description: "Writing a text file in JavaScript often pertains to creating and saving\
  \ data in a simple, readable format for logging, exporting user input, or\u2026"
lastmod: '2024-03-13T22:45:00.450904-06:00'
model: gpt-4-0125-preview
summary: Writing a text file in JavaScript often pertains to creating and saving data
  in a simple, readable format for logging, exporting user input, or configuration
  purposes.
title: Writing a text file
weight: 24
---

## How to:
In a Node.js environment, you can use the built-in `fs` (File System) module to write text files. This example demonstrates writing text to a file asynchronously:

```javascript
const fs = require('fs');

const data = 'Hello, World! This is text to be written into a file.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('File has been written.');
});
```

Sample output:
```
File has been written.
```

For synchronous file writing, use `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('File has been written.');
} catch (error) {
  console.error('Error writing file:', error);
}
```

In modern web browsers, the File System Access API introduces the ability to read and write files. However, its use is subject to user permissions. Here's how to create and write to a file:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! This is browser text file writing.');
  await writable.close();
}
```

For more complex scenarios or when working with large files, you might opt for third-party libraries like `FileSaver.js` for browsers:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! This is text from FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Remember, writing files on the client-side (in browsers) is restricted due to security concerns, and any operation that requires saving to the user's local disk will usually require their explicit permission.
