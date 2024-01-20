---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (`stderr`) means sending error messages and diagnostics to a dedicated stream, separate from regular output (`stdout`). Programmers do this for clean separation of normal output from errors, making program output easier to handle and debug.

## How to:
C++ uses `cerr` for writing to `stderr`. Here's the usage:

```cpp
#include <iostream>

int main() {
    std::cout << "This is regular output" << std::endl;
    std::cerr << "This is an error message" << std::endl;
    return 0;
}
```

Sample output might look like this:

```
This is regular output
This is an error message
```

Even if you redirect `stdout`, `stderr` keeps showing in the terminal:

```cpp
// Redirect stdout to a file, but stderr still shows up in the terminal
int main() {
    freopen("output.txt", "w", stdout);
    std::cout << "This wont show on terminal" << std::endl;
    std::cerr << "This will show in terminal" << std::endl;
    fclose(stdout);
    return 0;
}
```

## Deep Dive:
In UNIX-like systems, `stderr` was introduced to separate program output (`stdout`) from error messages, with each having its own file descriptor (1 for `stdout`, 2 for `stderr`). Alternatives to `cerr` are using `fprintf(stderr, ...)` in C or writing directly to file descriptor 2. Internally, `cerr` is an instance of `ostream` and is unbuffered to ensure immediate error output without waiting for the buffer to fill up.

## See Also:
- [cppreference std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [GNU C Library: Standard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Redirecting stdout and stderr](http://www.cplusplus.com/reference/cstdio/freopen/)