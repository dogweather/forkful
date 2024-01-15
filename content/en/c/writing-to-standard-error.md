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

## Why
Is your code producing errors or unexpected results? Or do you want to add custom error messages to make debugging easier? Then writing to standard error in C is the way to go. It allows you to output error messages to the console or terminal, providing useful information for developers and users alike.

## How To
Writing to standard error in C is a simple process. First, include the `stdio.h` header file in your code. Then, use the `fprintf()` function to print your error message to `stderr`, which is the standard error stream.

```
#include <stdio.h>

int main()
{
  // Print error message to standard error
  fprintf(stderr, "*** Error: Something went wrong ***\n");

  return 0;
}
```

The `fprintf()` function takes in multiple arguments, including the `stderr` file pointer, the error message string, and any additional values to be formatted into the message. The `\n` at the end will ensure that the message is printed on a new line.

When running this program, the error message will appear on the console or terminal instead of the standard output.

**Output:**
```
*** Error: Something went wrong ***
```

## Deep Dive
In C, there are three standard streams that can be used for input/output - `stdin`, `stdout`, and `stderr`. `stdin` is used for accepting input, `stdout` is used for outputting regular data, and `stderr` is used for error messages.

By default, `stderr` is connected to the console or terminal, allowing for error messages to be displayed during program execution. However, it can also be redirected to a file for logging purposes. This can be done by using the "> " symbol in the command line, followed by the name of the file where you want the error messages to be saved.

`./my_program > error_log.txt`

Keep in mind that `stderr` is unbuffered, meaning that the messages will be displayed immediately without waiting for the program to finish execution.

## See also
- [C fprintf() function](https://www.geeksforgeeks.org/fprintf-in-c/)
- [Error Handling in C](https://www.tutorialspoint.com/error-handling-in-c)
- [Redirecting Standard Output/Standard Error to Files in Linux](https://www.computerhope.com/unix/uredirect.htm)