---
title:                "Writing to standard error"
html_title:           "Python recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Writing to Standard Error in Python

## What & Why?
Standard error (`stderr`) is a special type of output stream typically used by programs to output error messages or diagnostics. It helps developers differentiate regular output from error messages and is particularly useful for debugging and for logs.

## How to:
Writing to stderr in Python is straightforward. Use the built-in `sys` module:

```Python
import sys

sys.stderr.write("This is an error message\n")
```

The string you put inside `write()` will be printed to stderr. It would be a good practice to include `\n` at the end of the message to ensure it ends with a newline.

The output will be like:

```Python
This is an error message
```

## Deep Dive
Historically, stderr originated in the Unix tradition, and Python acquired it naturally from there. It's important to note that using stderr does not stop the execution of a program, but does express a statement that an error has occurred.

Although `sys.stderr.write()` is one common way to print to stderr, `print()` function can direct output to stderr with the `file` parameter:

```Python
print('This is an error message', file=sys.stderr)
```

Underneath the surface, Python's `sys.stderr` and `sys.stdout` are file objects representing stderr and stdout (standard output) respectively. When these are called, the interpreter employs the underlying operating system routines to complete the task.

## See Also
For more information, take a look at:
- [Python sys module](https://docs.python.org/3/library/sys.html)
- [Python built-in print()](https://docs.python.org/3/library/functions.html#print)
- [Differentiate stdout and stderr](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))