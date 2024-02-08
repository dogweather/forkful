---
title:                "Odczytywanie pliku tekstowego"
aliases:
- pl/javascript/reading-a-text-file.md
date:                  2024-01-20T17:54:31.176356-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
JavaScript (JS) often reads text files to process data. Programmers do it to pull in configurations, fetch content, or parse external data.

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
