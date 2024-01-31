---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (stderr) is outputting text to the error stream. It separates normal output (stdout) from errors, allowing easier debugging and log analysis.

## How to:

```javascript
// Writing a simple error message to stderr
console.error('Error: Something went wrong');

// Example with formatted output
const errorCode = 404;
console.error(`Error: Page not found - Code ${errorCode}`);
```

Sample output:
```
Error: Something went wrong
Error: Page not found - Code 404
```

## Deep Dive
Historically, Unix-like systems differentiate between standard output and standard error to allow separate handling of regular messages and error messages. While `console.log` in Javascript writes to stdout, `console.error` specifically writes to stderr. 
Alternatives for writing to stderr include using `process.stderr.write()` which doesn't include a newline character at the end, unlike `console.error`.
Implementation-wise, when writing Node.js scripts, the output to `console.error()` can be redirected separately from `console.log()` when executing a script from the command line, which can be useful for logging errors to a different file.

## See Also
- MDN Web Docs on Console: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Node.js documentation on `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Explanation of stdout vs stderr: https://www.jstor.org/stable/25860673
