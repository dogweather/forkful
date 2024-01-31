---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file means saving data in a file with a `.txt` extension. Programmers do this for logging, configuration, or saving simple data without needing a database.

## How to:

TypeScript, being a superset of JavaScript, doesn’t have its own file system module, but it can use Node.js for this task. Ensure you have Node.js installed and then, let’s roll:

```typescript
// Import the 'fs' module to interact with the file system
import { writeFile } from 'fs';

// The content you want to write
const content = 'Hello, World!';

// Function to write content to a file
const writeTextToFile = (filePath: string, content: string): void => {
  writeFile(filePath, content, (err) => {
    if (err) {
      console.error('Error writing file:', err);
    } else {
      console.log('File written successfully');
    }
  });
};

// Use the function to write to 'output.txt'
writeTextToFile('./output.txt', content);
```

Sample output:
```
File written successfully
```

## Deep Dive

Historically, writing to text files is as old as computing itself for storage or communication between programs. Before databases became prevalent, flat files were common. Now, databases have largely taken this role, but text files are still vital for their simplicity.

Alternatives to the Node.js 'fs' module include:

- The new 'fs/promises' for Promise-based functions.
- Using 'fs-extra' for convenience methods.
- 'stream' module for dealing with large files.

The 'writeFile' method shown above works well for small to medium-sized files. For larger files or streams of data, you might want to use streams to avoid loading everything into memory.

## See Also

- Node.js File System API: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- TypeScript official page: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- 'fs-extra' library: [https://github.com/jprichardson/node-fs-extra](https://github.com/jprichardson/node-fs-extra)
- MDN Web Docs on Streams: [https://developer.mozilla.org/en-US/docs/Web/API/Streams_API](https://developer.mozilla.org/en-US/docs/Web/API/Streams_API)
