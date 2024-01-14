---
title:                "C recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is a necessary skill for any C programmer. It allows for error handling and debugging messages to be displayed, providing crucial information for troubleshooting and improving the overall functionality of the program.

## How To

Writing to standard error in C is a simple process that involves using the `fprintf()` function. This function takes in two parameters - the first being the standard error output `stderr` and the second being the message that needs to be displayed.

```
#include <stdio.h>

int main()
{
    fprintf(stderr, "This is an error message.\n");
    return 0;
}
```

Running this code will display the error message on the standard error output, which is often the command line or terminal window. The `\n` at the end of the message ensures that the message is displayed on a new line.

## Deep Dive

In deeper technical terms, writing to standard error is important because it allows for the separation of error messages from normal output in C programs. Other methods of displaying error messages such as `printf()` can cause issues because the messages may be mixed in with normal output, making it difficult to identify and troubleshoot errors.

Another advantage of writing to standard error is that it can be redirected to a different output file using the `2>` operator in the command line. This allows for error messages to be saved for future reference or analysis.

## See Also

- [C Error Handling](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
- [Standard Error Output](https://www.geeksforgeeks.org/pipe-stderr-stdout-c/)
- [Redirecting Standard Error in C](https://www.linuxjournal.com/content/redirecting-stderr-stdout)

Writing to standard error may seem like a minor detail, but it is an important aspect of C programming. By mastering this skill, you can improve the functionality and effectiveness of your code, making you a better programmer overall.