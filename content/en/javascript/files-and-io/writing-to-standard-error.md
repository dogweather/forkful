---
title:                "Writing to standard error"
aliases:
- /en/javascript/writing-to-standard-error.md
date:                  2024-02-03T19:03:32.441454-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing to standard error"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (stderr) in JavaScript is about directing error messages or any critical information to a specific, separate stream, which is especially useful in Unix-like environments for logging and debugging purposes. Programmers do this to differentiate normal program output from error messages, allowing for cleaner output management and easier error monitoring.

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
