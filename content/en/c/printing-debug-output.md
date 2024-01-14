---
title:    "C recipe: Printing debug output"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the software development process, and printing debug output is a crucial tool for identifying and fixing errors in code. It allows developers to see the state of variables, the flow of execution, and any potential errors that may occur during runtime. Without debug output, it can be challenging to pinpoint the exact cause of a problem, leading to longer debugging times and frustration.

## How To

To print debug output in C, we use the `printf()` function from the standard input-output library `stdio.h`. It takes in a string of characters (known as format string) and optional arguments, which are then formatted and displayed on the screen. Let's see an example:

```
#include <stdio.h>

int main()
{
    int num = 10;
    printf("The value of num is %d\n", num);
    return 0;
}
```

In the above code, we use the `%d` specifier in the format string to indicate that we want to print an integer value, which is provided as an argument after the comma. The output of this code would be:

```
The value of num is 10
```

We can also use other specifiers, such as `%f` for floating-point values, `%c` for characters, and `%s` for strings. Additionally, we can use escape sequences like `\n` or `\t` to format the output.

## Deep Dive

While `printf()` is the standard way to print debug output, there are other options available, such as `fprintf()` and `sprintf()`. `fprintf()` allows us to print to a specific output stream, such as a file, while `sprintf()` allows us to store the formatted output in a string variable instead of printing it on the screen.

Another useful function is `assert()`, which is used for debugging by checking if a condition is true and stopping the program if it is not. This can help catch bugs early on, instead of spending hours trying to find the cause of a runtime error.

Using preprocessor macros like `#define DEBUG` and using an `#ifdef` statement, we can selectively print debug output only when needed. This is particularly useful when working with large projects where printing too much output can be overwhelming.

## See Also

- [Debugging in C - GeeksforGeeks](https://www.geeksforgeeks.org/debugging-c-set-1-simple-macro/)
- [C Debugging Tools for Linux - IBM Developer](https://developer.ibm.com/technologies/systems/articles/au-dmdebug-linux/)
- [Debugging Tutorials for C/C++ Programmers - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)