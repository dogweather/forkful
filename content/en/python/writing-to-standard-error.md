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

## What & Why?

Writing to standard error in Python is a way for programmers to communicate specific information or errors to the user within their code. It allows for separate output from regular print statements and can be useful for debugging and troubleshooting.

## How to:

```Python
import sys
sys.stderr.write("This message will be printed to standard error")
```

Running this code will produce the output:

```
This message will be printed to standard error
```

## Deep Dive:

Writing to standard error originated from the UNIX operating system in the 1970s. It was standardized as a way for programs to communicate error messages to the user, separate from regular output. This is helpful for troubleshooting and debugging, as error messages can be easily identified and separated.

An alternative to writing to standard error is using the ```logging``` module in Python. This allows for more advanced logging capabilities, such as different levels of severity for messages.

In order to write to standard error, the ```sys``` module must be imported and the ```sys.stderr.write()``` function must be used. The ```stderr``` attribute of the ```sys``` module refers to the standard error stream.

## See Also:

The [official Python documentation](https://docs.python.org/3/library/sys.html#sys.stderr) for the ```sys.stderr``` attribute.

The [Python logging tutorial](https://docs.python.org/3/howto/logging.html) for more information on advanced logging techniques.

The [wikipedia entry](https://en.wikipedia.org/wiki/Standard_streams) on standard streams, which includes information on standard error and its history.