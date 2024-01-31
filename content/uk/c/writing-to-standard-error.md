---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
simple_title:         "Запис в стандартний потік помилок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
(## Що таке & Чому?)

Writing to standard error (stderr) means sending error messages and diagnostics there. Programmers do it to separate regular output from errors, making it easier to handle them differently.

## How to:
(## Як це зробити:)

Here's how to write to stderr in C:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "An error occurred!\n");
    return 0;
}
```

Sample output when an error occurs:

```
An error occurred!
```

## Deep Dive
(## Поглиблений аналіз)

Writing to stderr dates back to early Unix systems, keeping a clear distinction between standard output (stdout) and standard error. Alternatives include writing to log files or using syslog on Unix-like systems. Internally, stderr is a FILE* stream, buffered differently from stdout, and is typically unbuffered to speed up error reporting.

## See Also
(## Дивіться також)

- GNU C Library Manual on Standard Streams: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- POSIX standard definition for stderr: https://pubs.opengroup.org/onlinepubs/9699919799/functions/stderr.html
- Advanced error handling with errno: http://man7.org/linux/man-pages/man3/errno.3.html
