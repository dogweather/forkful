---
date: 2024-01-20 17:54:33.767948-07:00
description: "Reading a text file is pulling info from a .txt document into your program.\
  \ Programmers do it to access and manipulate data: config settings, logs,\u2026"
lastmod: '2024-03-13T22:45:00.450029-06:00'
model: gpt-4-1106-preview
summary: "Reading a text file is pulling info from a .txt document into your program.\
  \ Programmers do it to access and manipulate data: config settings, logs,\u2026"
title: Reading a text file
weight: 22
---

## What & Why?
Reading a text file is pulling info from a .txt document into your program. Programmers do it to access and manipulate data: config settings, logs, exports, and so on. Plain and simple.

## How to:
Here's how you go about reading a text file in current JavaScript:

**Using Node.js with Promises (Async/Await)**:

```javascript
const fs = require('fs').promises;

async function readFile(filePath) {
  try {
    const data = await fs.readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error('Got an error trying to read the file:', error);
  }
}

readFile('example.txt');
```

Sample output (contents of `example.txt`):

```
Hello, this is a text file!
```

**Using fetch API in the browser**:

```javascript
async function fetchTextFile(fileUrl) {
  try {
    const response = await fetch(fileUrl);
    const text = await response.text();
    console.log(text);
  } catch (error) {
    console.error('Oops, something went wrong fetching the file:', error);
  }
}

fetchTextFile('example.txt');
```

## Deep Dive
Originally, reading files in JavaScript was mostly a server-side affair, dealt with by Node.js. As JS danced into browsers with HTML5, APIs like `FileReader` and `fetch` arrived, making client-side file reading possible without a sweat.

Alternatives? Oh, there are a few. Streams can handle big files without hogging memory. Workers prevent UI freeze-ups. Libraries make complex tasks easier. Each has its place.

Under the hood, file reading may involve buffer management, character encoding (UTF-8, etc.), and error handling. Be mindful of security, too; browsers restrict file access for good reasons. 

## See Also
Take your learning further with these resources:

- MDN's FileReader API Doc: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Node.js File System Docs: [Node.js fs](https://nodejs.org/api/fs.html)
- Stream API for big files: [Node.js stream](https://nodejs.org/api/stream.html)
- Understanding fetch API: [MDN fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
