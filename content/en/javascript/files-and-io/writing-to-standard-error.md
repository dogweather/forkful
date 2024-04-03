---
date: 2024-02-03 19:03:32.441454-07:00
description: "How to: In Node.js, writing to stderr can be accomplished using the\
  \ `console.error()` method or by writing directly to `process.stderr`. Here are\
  \ examples\u2026"
lastmod: '2024-03-13T22:45:00.449112-06:00'
model: gpt-4-0125-preview
summary: In Node.js, writing to stderr can be accomplished using the `console.error()`
  method or by writing directly to `process.stderr`.
title: Writing to standard error
weight: 25
---

## How to:
In Node.js, writing to stderr can be accomplished using the `console.error()` method or by writing directly to `process.stderr`. Here are examples demonstrating both approaches:

```javascript
// Using console.error()
console.error('This is an error message.');

// Directly writing to process.stderr
process.stderr.write('This is another error message.\n');
```

Sample output for both methods would appear in the stderr stream, not mingling with stdout:
```
This is an error message.
This is another error message.
```

For more sophisticated or application-specific logging, many JavaScript programmers use third-party libraries like `winston` or `bunyan`. Here's a quick example using `winston`:

First, install `winston` via npm:
```shell
npm install winston
```

Then, configure `winston` to log errors to stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Logging an error message
logger.error('Error logged through winston.');
```

This setup ensures that when you log an error using `winston`, it directs to stderr, helping maintain clear separation between standard and error outputs.
