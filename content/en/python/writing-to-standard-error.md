---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error, often referenced as stderr, is a predefined file stream for logging error messages. Programmers use it to separate regular program output from error messages, which makes debugging easier.

## How to:
To write to stderr in Python:

```Python
import sys

print("This is a normal message.")
sys.stderr.write("This is an error message.\n")
```

Sample Output:
```
This is a normal message.
This is an error message.
```

Note that while `print()` adds a newline by default, `sys.stderr.write()` does not—you need to include `\n` to start a new line.

## Deep Dive
Historically, standard streams were introduced in Unix. There are three: standard input (`stdin`), standard output (`stdout`), and standard error (`stderr`). In Python, the `sys` module provides access to these streams. While `stdout` is typically used for the main output of a program, `stderr` is reserved for error messages and diagnostics.

Alternatives to `sys.stderr.write()` include using `print()` with the `file` argument:

```Python
print("This is an error message.", file=sys.stderr)
```

This performs similarly but leverages `print()`'s user-friendly features. Regarding internal mechanics, both methods end up making system-level write calls to the respective stream.

## See Also
- Python documentation for the sys module: https://docs.python.org/3/library/sys.html
- Unix Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
- Discussion on stderr usage: https://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python
