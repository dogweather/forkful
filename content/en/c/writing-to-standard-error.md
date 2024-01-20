---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) is how you output error messages and diagnostics in C. It's separate from standard output (stdout) to let you handle regular and error outputs differently, such as logging errors or streamline debugging.

## How to:

Here's how to write to stderr, using standard library functions.

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "An error occurred!\n");
    return 0;
}
```

Sample output:

```
An error occurred!
```

Use `perror` when you want to add a message about the last system error:

```C
#include <stdio.h>
#include <errno.h>

int main() {
    fopen("nonexistentfile.txt", "r");

    if (errno) {
        perror("File open failed");
    }

    return 0;
}
```

Sample output:

```
File open failed: No such file or directory
```

## Deep Dive

Historically, separating stderr from stdout helps when running programs from a shell. Standard output can be redirected to a file or another program while standard error remains visible in the terminal. This distinction is crucial in Unix-based systems.

Alternatives to `fprintf` or `perror` include directly writing to the file descriptor, like `write(2, "Error\n", 6);`, though it's less common since it's lower-level.

Regarding implementation, stderr is a `FILE` pointer that's buffered. But, unlike stdout, it's typically set to unbuffered mode so that error messages are more immediate, which is key for understanding program issues as they occur.

## See Also

- [GNU C Library: Standard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Write Syscall Man Page](https://man7.org/linux/man-pages/man2/write.2.html)