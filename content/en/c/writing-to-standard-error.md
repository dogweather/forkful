---
title:    "C recipe: Writing to standard error"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error in C programming may seem like a small technical detail, but it can have a big impact on the overall functionality and user experience of your program. By understanding how to properly write to standard error, you can improve error handling and make debugging easier.

## How To

In C programming, standard error is known as "stderr" and is used for displaying error messages to the user. Instead of using the standard output stream, "stdout", which is typically used for regular program output, writing to stderr ensures that error messages are displayed separately and not mixed in with regular output.

To write to stderr in C, you first need to include the "stdio.h" header file and use the "fprintf" function. Here's an example of a program that uses fprintf to write an error message to stderr:

```C
#include <stdio.h>

int main() {

   // some code that may result in an error

   if(error_condition) {
      fprintf(stderr, "An error has occurred.\n");
   }

   return 0;
}
```

The error message in this program will be printed to the console separately from any regular output, making it easier for the user to identify and troubleshoot. Additionally, you can use the "perror" function to print a custom error message along with the system's error message, which can provide more context for the error:

```C
#include <stdio.h>
#include <errno.h>

int main() {

   // some code that may result in an error

   if(error_condition) {
      perror("An error has occurred.");
   }

   return 0;
}
```

This will produce an output like "An error has occurred. No such file or directory.", indicating the specific error that occurred.

## Deep Dive

Under the hood, stderr is a file stream that is associated with the standard error output device. This could be the console or a log file, depending on how the program is being executed. In C programming, "stderr" is actually a predefined file pointer that points to the standard error stream, making it easy to use in your code without having to declare it yourself.

It's important to note that the standard error stream has a default behavior of being buffered, meaning that any output written to it will not be immediately displayed until a new line is encountered or the buffer is flushed. This can cause issues when trying to debug errors in real-time, as the error messages may not be displayed until after the program has finished executing. To fix this, you can use the "fflush" function to manually flush the buffer and display the error message immediately:

```C
#include <stdio.h>

int main() {

   // some code that may result in an error

   if(error_condition) {
      fflush(stderr);
      fprintf(stderr, "An error has occurred.\n");
   }

   return 0;
}
```

By understanding the inner workings of stderr and how to properly use it in your code, you can ensure that error messages are displayed in a clear and timely manner.

## See Also
- [The C Standard Library](https://en.wikipedia.org/wiki/C_standard_library)
- [An Introduction to Standard Error in C Programming](https://brennan.io/2015/01/16/write-to-stderr/)
- [The stdio.h header file in C](https://www.tutorialspoint.com/c_standard_library/stdio_h.htm)