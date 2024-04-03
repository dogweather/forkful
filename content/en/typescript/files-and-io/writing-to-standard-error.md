---
date: 2024-02-03 19:03:34.228225-07:00
description: "How to: TypeScript, being a superset of JavaScript, relies on the underlying\
  \ JS runtime environment (like Node.js) for writing to stderr. Here's how you\u2026"
lastmod: '2024-03-13T22:44:59.873267-06:00'
model: gpt-4-0125-preview
summary: TypeScript, being a superset of JavaScript, relies on the underlying JS runtime
  environment (like Node.js) for writing to stderr.
title: Writing to standard error
weight: 25
---

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
