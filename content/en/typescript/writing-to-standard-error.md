---
title:                "Writing to standard error"
date:                  2024-01-19
simple_title:         "Writing to standard error"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (`stderr`) sends error messages and diagnostics separate from standard output (`stdout`). Programmers do this to debug and log errors without cluttering regular program output.

## How to:

In TypeScript, you can write to `stderr` using `console.error` or `process.stderr.write`. Here's both in action:

```TypeScript
console.error("This is an error message to stderr");

process.stderr.write("This is another error message to stderr\n");
```

Sample output for both lines:

```
This is an error message to stderr
This is another error message to stderr
```

## Deep Dive

Historically, separating `stdout` and `stderr` let Unix users direct output and errors to different destinations. You could log errors for analysis while having clean output data. Alternatives to writing directly to `stderr` include logging libraries or frameworks that offer more control and features. Implementation-wise, `console.error` wraps around `process.stderr.write` with additional formatting capabilities, so using `console.error` is generally more convenient for simple messages.

## See Also

- Node.js documentation on console: https://nodejs.org/api/console.html
- Node.js process standard streams: https://nodejs.org/api/process.html#process_process_stderr
- Discussion on `console.error` vs `process.stderr.write`: https://stackoverflow.com/questions/4976466/difference-between-process-stdout-write-and-console-log-in-node-js
