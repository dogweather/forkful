---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in C yields access to parameters directly from the terminal at program runtime. It's useful for manipulating how your program operates without editing your source code.

## How to

In C, command line arguments are read using the main function. Let's see this in action.

```C
#include <stdio.h>

int main(int argc, char *argv[]){
    for(int i = 0; i < argc; i++) {
        printf("Arg %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Suppose you compile and run with `./program Hello World`, it'd output:

```
Arg 0: ./program
Arg 1: Hello
Arg 2: World
```

Here `argc` is the count of command line arguments, `argv` an array storing them, and `argv[0]` the program's name itself.

## Deep Dive

This command line argument reading strategy originated in early Unix systems. It means "argument vector", depicting the linear structure used to store arguments.

A reasonable alternative to `argv` is using `getopt()`. It neatly deals with more complex command line parameters but remember, it's heavier and POSIX-specific.

Also note: `argv[argc]` is always null, and `argv` is not modifiable: any changes to it outlast the function scope.

## See Also

- [Official ISO C Standard](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf)
- [Using argc and argv in C programming](https://www.cprogramming.com/tutorial/c/lesson14.html)