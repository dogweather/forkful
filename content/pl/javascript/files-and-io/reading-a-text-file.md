---
date: 2024-01-20 17:54:31.176356-07:00
description: "How to: (Jak to zrobi\u0107:) Reading text files in JS depends on the\
  \ environment: browser or Node.js. Here are both."
lastmod: '2024-03-13T22:44:35.813748-06:00'
model: gpt-4-1106-preview
summary: Reading text files in JS depends on the environment.
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to: (Jak to zrobić:)
Reading text files in JS depends on the environment: browser or Node.js. Here are both:

### Browser:
```javascript
// HTML5 File API for reading files in-browser
document.querySelector('input[type=file]').addEventListener('change', event => {
  const reader = new FileReader();
  
  reader.onload = function(e) {
    console.log('File content:', e.target.result);
  };

  reader.readAsText(event.target.files[0]);
});
```

### Node.js:
```javascript
const fs = require('fs');

// Async read from file system
fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading file:', err);
    return;
  }
  console.log('File content:', data);
});
```

### Sample Output:
```
File content: Here is the text inside your file.
```

## Deep Dive (Dogłębna analiza):
Historically, JS was client-side only, and file access was severely restricted for security reasons. Node.js introduced server-side JS, allowing file system access.

Alternatives:
- `fetch` in browsers for remote files.
- Streams in Node.js for big files to prevent memory overload. 

Implementation:
- The FileReader API in browsers works asynchronously.
- Node.js `fs.readFile` reads the entire file into memory, but you can use `fs.createReadStream` for a more efficient approach.

## See Also (Zobacz również):
- MDN Web Docs on FileReader API: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Node.js File System (fs) module: [Node.js fs](https://nodejs.org/api/fs.html)
- Node.js Stream API: [Node.js Stream](https://nodejs.org/api/stream.html)
