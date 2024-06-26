---
date: 2024-02-03 19:02:35.601558-07:00
description: "How to: TypeScript, when run in a Node.js environment, allows you to\
  \ check if a directory exists by using the `fs` module, which provides the\u2026"
lastmod: '2024-03-13T22:44:59.871565-06:00'
model: gpt-4-0125-preview
summary: TypeScript, when run in a Node.js environment, allows you to check if a directory
  exists by using the `fs` module, which provides the `existsSync()` function or the
  asynchronous `access()` function combined with `constants.F_OK`.
title: Checking if a directory exists
weight: 20
---

## How to:
TypeScript, when run in a Node.js environment, allows you to check if a directory exists by using the `fs` module, which provides the `existsSync()` function or the asynchronous `access()` function combined with `constants.F_OK`.

### Using `fs.existsSync()`:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Directory exists.');
} else {
  console.log('Directory does not exist.');
}
```

### Using `fs.access()` with `fs.constants.F_OK`:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Directory does not exist.');
    return;
  }
  console.log('Directory exists.');
});
```

**Sample Output** for both methods, assuming the directory does exist:
```
Directory exists.
```

And if it doesn't:
```
Directory does not exist.
```

### Using a Third-Party Library - `fs-extra`:
`fs-extra` is a popular third-party library that enhances the built-in `fs` module and provides more convenient functions.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Directory exists: ${exists}`);
});
```

**Sample Output** when the directory exists:
```
Directory exists: true
```

And if it doesn't:
```
Directory exists: false
```
