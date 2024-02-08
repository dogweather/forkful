---
title:                "Читання текстового файлу"
aliases:
- uk/javascript/reading-a-text-file.md
date:                  2024-01-20T17:54:30.101720-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Reading text files means grabbing text data from a .txt (or similar) file. Programmers read files to use data like configuration settings, import user data, or to analyze text outside of the application itself.

## How to: (Як зробити:)
Use `fetch` for web files, `FileReader` for client-side operations, or `fs` in Node.js. Here’s the quick code.

```Javascript
// Web files:
fetch('data.txt')
  .then(response => response.text())
  .then(text => console.log(text));

// Client-side files:
document.getElementById('input-file').addEventListener('change', event => {
  const file = event.target.files[0];
  const reader = new FileReader();
  reader.onload = e => console.log(e.target.result);
  reader.readAsText(file);
});

// Node.js:
const fs = require('fs');

fs.readFile('data.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Sample Output for each:
```
Hello, this is a text file content!
```

## Deep Dive (Поглиблене занурення):
Historically, JavaScript was browser-only; Node.js allowed server-side file operations. Alternatives include `XMLHttpRequest` for browsers (but `fetch` is modern). Implementation details matter: know when to use asynchronous (`fs.readFile`) versus synchronous (`fs.readFileSync`) for Node.js, or how to handle file reading in chunks for big files.

## See Also (Дивіться також):
- MDN Docs on `fetch()`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Node.js `fs` module docs: https://nodejs.org/api/fs.html
- FileReader API docs: https://developer.mozilla.org/en-US/docs/Web/API/FileReader
