---
date: 2024-02-03 19:02:38.194848-07:00
description: "Checking if a directory exists in JavaScript is essential for file manipulation\
  \ tasks, enabling scripts to verify directory presence before reading from\u2026"
lastmod: '2024-03-11T00:14:34.319649-06:00'
model: gpt-4-0125-preview
summary: "Checking if a directory exists in JavaScript is essential for file manipulation\
  \ tasks, enabling scripts to verify directory presence before reading from\u2026"
title: Checking if a directory exists
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists in JavaScript is essential for file manipulation tasks, enabling scripts to verify directory presence before reading from or writing to it. This operation prevents errors and ensures smoother program execution, particularly in applications that dynamically handle files or directories based on user input or external data sources.

## How to:
In Node.js, since JavaScript itself doesn't have direct access to the file system, the `fs` module is typically used for such operations. Here's a simple way to check if a directory exists using `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Check if the directory exists
if (fs.existsSync(directoryPath)) {
  console.log('Directory exists.');
} else {
  console.log('Directory does not exist.');
}
```
**Sample Output:**
```
Directory exists.
```
Or, for a non-blocking asynchronous approach, use `fs.promises` with `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Directory exists.');
  } catch (error) {
    console.log('Directory does not exist.');
  }
}

checkDirectory('./sample-directory');
```
**Sample Output:**
```
Directory exists.
```

For projects that make heavy use of file and directory operations, the `fs-extra` package, an extension of the native `fs` module, offers convenient additional methods. Here's how you can achieve the same with `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Check if the directory exists
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Directory exists.' : 'Directory does not exist.'))
  .catch(err => console.error(err));
```
**Sample Output:**
```
Directory exists.
```

This approach enables clean, readable code that seamlessly integrates with modern JavaScript practices.
