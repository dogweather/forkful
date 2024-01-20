---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in JavaScript usually means creating and saving data to a file in a human-readable format. Programmers do it to persist data, like settings, logs, or user output.

## How to:

JavaScript in a browser doesn't have direct access to the file system for security reasons. But you can create a text file and prompt the user to save it:

```javascript
function downloadTextFile(text, filename) {
  const blob = new Blob([text], { type: 'text/plain' });
  const a = document.createElement('a');
  a.download = filename;
  a.href = window.URL.createObjectURL(blob);
  a.dataset.downloadurl = ['text/plain', a.download, a.href].join(':');
  a.style.display = "none";
  document.body.appendChild(a);  // Append anchor to body.
  a.click();
  
  document.body.removeChild(a);  // Cleanup anchor after usage.
  window.URL.revokeObjectURL(a.href);  // Release blob URL.
}

// Usage:
downloadTextFile('Hello, world!', 'example.txt');
```

Node.js provides a more straightforward way to write files via the `fs` module:

```javascript
const fs = require('fs');

fs.writeFile('example.txt', 'Hello, world!', (err) => {
  if (err) throw err;
  console.log('File has been saved!');
});
```

## Deep Dive

Historically, JavaScript was confined to the browser without file system access. Node.js changed that game by exposing server-side capabilities.

Alternatives to `fs.writeFile` include `fs.writeFileSync` for synchronous operations and `fs.promises.writeFile` for promise-based asynchronous control.

Node's `fs` methods handle buffers and streams—tools addressing large file handling and network communication.

## See Also

- Node.js File System Docs: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN - Blob: [https://developer.mozilla.org/en-US/docs/Web/API/Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
- MDN - JavaScript Guide: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)