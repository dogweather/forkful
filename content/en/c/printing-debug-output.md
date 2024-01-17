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

## What & Why?

Printing debug output is a common technique used by programmers during the development and testing phase of their code. It involves printing out specific information about the state of the program or its variables to help identify and fix any errors or issues.

Debug output is helpful because it allows programmers to see what is happening behind the scenes of their code and to track the flow of execution. This can be especially useful for finding and fixing hard-to-detect bugs.

## How to:

To print debug output in C, we use the ```printf()``` function. Let's look at a simple example:

```
#include <stdio.h>

int main()
{
    int num = 5;
    printf("The value of num is: %d\n", num);

    return 0;
}
```

In this code, the ```printf()``` function is used to print the value of the variable ```num``` to the console. The format specifier ```%d``` is used to specify that we want to print an integer value.

The output of this code would be:

```
The value of num is: 5
```

## Deep Dive:

Debug output has been used since the early days of programming and has become an essential tool for debugging code. It allows programmers to track the values and changes in variables during program execution, as well as identify potential issues with logic or algorithms.

An alternative to using ```printf()``` for debug output is the use of a debugger tool, such as GDB. However, debug output can be more convenient for quickly checking specific values without having to go through a full debugging process.

In terms of implementation, debug output can be inserted directly into the code at specific points, or it can be triggered by certain conditions using conditional statements. Additionally, advanced techniques such as logging can also be used for more detailed and persistent debug output.

## See Also:

- [C Debugging: Tips, Types and Tools](https://stackify.com/c-debugging-tips/)
- [GDB - The GNU Project Debugger](https://www.gnu.org/software/gdb/)
- [The Power of Printing in Debugging](https://blog.acolyer.org/2015/03/24/the-power-of-printing-in-debugging/)