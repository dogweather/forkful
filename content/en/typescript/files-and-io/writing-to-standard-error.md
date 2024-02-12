---
title:                "Writing to standard error"
aliases: - /en/typescript/writing-to-standard-error.md
date:                  2024-02-03T19:03:34.228225-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing to standard error"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
In TypeScript, writing to standard error (stderr) is a process of sending error messages or logs directly to the error output stream of the environment (e.g., the console in node.js or a web browser). This is essential for diagnosing problems without interfering with the standard output (stdout) used typically for program data, ensuring that error handling and logging are managed efficiently and cohesively.

## How to:
TypeScript, being a superset of JavaScript, relies on the underlying JS runtime environment (like Node.js) for writing to stderr. Here's how you can do it directly:

```typescript
console.error("This is an error message.");
```

Sample output to stderr:
```
This is an error message.
```

In a Node.js environment, you can also use the `process.stderr.write()` method for more low-level writing:

```typescript
process.stderr.write("Low level error message.\n");
```

Sample output to stderr:
```
Low level error message.
```

For more structured error logging, you might use popular third-party libraries such as `winston` or `pino`. Here’s how to log errors using `winston`:

First, install `winston`:

```bash
npm install winston
```

Then use it in your TypeScript file:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Error logged using winston.');
```

This will write the error to both the console and a file named `error.log`. Remember, when writing to files, it's important to manage file permissions and rollover to prevent issues related to disk space.usage.
