---
title:                "C recipe: Writing to standard error"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, the ability to effectively communicate with the user is a crucial skill. Standard error, or stderr, is a commonly used channel for printing error messages and debugging information in C programming. Understanding how to write to stderr not only enhances the user experience but also helps in improving the overall functionality of the program. 

## How To

Writing to stderr in C is a simple process that involves using the function `fprintf` from the standard input/output (stdio) library. This function takes in two parameters - the first one being the file pointer `stderr`, and the second one being the string or message to be printed. Let's take a look at a code snippet to better understand this process:

```C
#include <stdio.h>

int main() {
    // Print a simple error message to stderr
    fprintf(stderr, "Oops! Something went wrong.\n");

    // Print an error message along with a variable value
    int errorCode = 404;
    fprintf(stderr, "Error: %d not found.\n", errorCode);

    return 0;
}
```

In the above example, we include the header file `stdio.h` to access the function `fprintf`. We then use `fprintf` to print two different error messages to stderr. The `\n` character is used to add a new line after each message. Notice how we can also include variables in the error message by using formatting specifiers (in this case, `%d` for integers). 

The code above will produce the following output when run:

```bash 
Oops! Something went wrong.
Error: 404 not found.
```

## Deep Dive

One key advantage of using stderr for error messages is that it is separate from the standard output (stdout). This means that even if the program is writing regular output to the console, the error messages will still be displayed clearly and separately. 

Another advantage is that stderr is unbuffered, meaning that the output is immediately displayed on the console without any delays. This makes it useful for real-time debugging and error handling. 

Keep in mind that while stderr is often used for error messages, it can also be used for printing any type of information. This can be helpful for displaying debugging information or program status updates to the user. 

## See Also

- [Using fprintf to write to an Output File](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [The role of stderr in C error handling](https://www.thegeekstuff.com/2012/06/c-error-handling/)
- [Exploring the Basics of Standard I/O in C](https://www.dmishra.com/2017/01/io-streams-c-programming-part-3.html)