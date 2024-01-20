---
title:                "Writing to standard error"
html_title:           "C recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
In C programming, writing to standard error is a method of printing error messages to the error output stream rather than the standard output stream. This allows for clearer separation of regular output and error messages, making it easier to identify and troubleshoot issues during program execution.

Programmers use this technique to provide more useful and informative error messages to users or to log errors for later debugging. By default, error messages are printed to the console using the standard error stream, which is usually displayed in red text to differentiate it from regular output.

## How to:
To write to standard error in C, the `stdio.h` header file must be included in your code. Then, the `fprintf()` function can be used to print to the standard error stream, specified by the stderr pointer.

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "This is an error message.");
  return 0;
}
```

This will print the error message "This is an error message." to the console, using the standard error stream. The output may vary based on your IDE or command-line interface, but it will typically be displayed in red text.

## Deep Dive:
Writing to standard error has been a fundamental aspect of programming since the early days of UNIX. This method can be seen as an alternative to using the `printf()` function, which prints to the standard output stream. While `printf()` can also be used to print error messages, it is better practice to use `fprintf()` for the reasons mentioned in the "What & Why?" section.

In some cases, programmers may choose to use a logging library, such as `log4c` or `log4cpp`, to handle error messages and other logs. These libraries allow for more control over the formatting and output of error messages, but they are not a replacement for writing to standard error.

The implementation details of writing to standard error may differ depending on the operating system. However, the general concept remains the same - using the `fprintf()` function to print to the standard error stream.

## See Also:
- [C Programming - Error Handling](https://www.geeksforgeeks.org/error-handling-c-programs/)