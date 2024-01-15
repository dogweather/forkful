---
title:                "Printing debug output"
html_title:           "C recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of software development, and one of the most common tools used in debugging is printing debug output. This allows developers to track the flow of their code, understand what is happening at each step, and identify any potential errors or bugs.

## How To

Printing debug output in C is a simple and effective way to aid in the debugging process. To do so, we can use the `printf()` function, which allows us to print formatted output to the standard output stream (usually the terminal).

Let's take a look at a simple example:

```C
#include <stdio.h>

int main() {
    int num1 = 10;
    int num2 = 5;

    // Print the values of num1 and num2
    printf("num1 = %d, num2 = %d\n", num1, num2);

    // Print the sum of num1 and num2
    printf("%d + %d = %d\n", num1, num2, num1 + num2);

    return 0;
}
```

Output:
```
num1 = 10, num2 = 5
10 + 5 = 15
```

In this example, we first declare two variables `num1` and `num2` and assign them the values 10 and 5 respectively. Then, using `printf()`, we print the values of these variables as well as their sum.

It's important to note that `printf()` uses format specifiers to print the values of variables. In the above example, `%d` is used to print integers, but there are many other format specifiers for different data types.

## Deep Dive

Printing debug output can also be useful for tracking the execution of a program. By strategically placing `printf()` statements in different parts of the code, we can get a better understanding of how our program is running and where any potential errors may be occurring.

Another technique for debugging with `printf()` is using conditional statements. For example, we can print a message only if a certain condition is met, which can help us pinpoint the exact location of a bug.

It's important to remember to remove all debug `printf()` statements before releasing the final version of our code, as it can impact the performance of our program.

## See Also

- [Debugging in C: Tips and Tricks](https://www.codeproject.com/Articles/15971/Debugging-in-C-Tips-and-Tricks)
- [Logging in C and C++ using the Standard Library](https://www.guru99.com/c-logging-programming.html)
- [How to Debug C Programs on Linux Using gdb](https://www.tecmint.com/debug-c-programs-in-linux-using-gdb/)