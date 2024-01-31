---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:59:16.497629-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is about making sure a folder is actually there before you read from it or write to it. Programmers do it to avoid errors, like trying to save a file to a nonexistent place – that's a definite no-go.

## How to:
In TypeScript, you'll usually use Node.js's `fs` module to check for a directory. Here's the quick way to do it:

```typescript
import { existsSync } from 'fs';

// Check if a directory exists
const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log(`Yep, it's there!`);
} else {
  console.log(`Nope, doesn't exist.`);
}
```

Output depends on the directory's existence:
```
Yep, it's there!
// or
Nope, doesn't exist.
```

## Deep Dive
Historically, folks used the asynchronous `fs.exists`, but it was deprecated because it had a pesky habit of causing coding mistakes, like check-then-act race conditions. `existsSync` is simpler and cuts out the callback mess.

As for alternatives, the `fs.statSync` or `fs.accessSync` methods can do the job too but require a bit more code:

```typescript
import { statSync } from 'fs';

try {
  const stats = statSync(directoryPath);
  if (stats.isDirectory()) {
    console.log('It exists indeed.');
  }
} catch (error) {
  if (error.code === 'ENOENT') {
    console.log('Nope, nowhere to be found.');
  }
}
```

Both `statSync` and `accessSync` throw errors if the path doesn't exist, so you'll need to handle that.

When using TypeScript, remember that these methods come from Node.js, not TypeScript itself. And TypeScript's role? Mainly, it just provides the types and makes sure you're using the methods correctly.

## See Also
- Node.js File System Docs: https://nodejs.org/api/fs.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Error Handling in Node.js: https://nodejs.org/en/knowledge/errors/what-are-the-error-conventions/
