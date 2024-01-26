---
title:                "Handling errors"
date:                  2024-01-21T21:19:20.700297-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors in C is about expecting the unexpected. It keeps programs from going haywire when they bump into problems. Programmers do it to handle mistakes gracefully and keep their code reliable.

## How to:

Let's see how to do this in C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }
    // Do something with the file
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Sample output when the file doesn't exist:
```
Error opening file: No such file or directory
```

## Deep Dive

In the early C days, error handling was barebones - mostly return codes and manual checks. Enter `errno`, a global variable updated when functions fail. It's not thread-safe by itself, though, so the newer `strerror` and `perror` functions were introduced for better error reporting.

Alternatives? Modern C isn't limited to `errno`. There's setjmp and longjmp for non-local jumps when disaster strikes. Some folks prefer defining their error codes, while others opt for exception-like structures in C++.

Implementation details can be thick. For example, `errno` is thread-safe in POSIX compliant systems due to the magic of Thread Local Storage (TLS). In embedded systems, where resources are precious, custom error handling code might be preferred over standard approaches that could bloat the software.

## See Also

- A detailed dive into `errno`: https://en.cppreference.com/w/c/error/errno
- For thread safety, see POSIX threads and errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- An intro to setjmp and longjmp: https://www.cplusplus.com/reference/csetjmp/
- For exception handling in C++, check out: https://isocpp.org/wiki/faq/exceptions
