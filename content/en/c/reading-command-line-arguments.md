---
title:                "Reading command line arguments"
date:                  2024-01-20T17:55:21.511217-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Arguably, every coder has bumped into command line arguments. They let users feed data to your program. Dialing them in can drastically change how your program behaves without altering the code—think of cheat codes for gamers, but for programmers.

## How to:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("You have entered %d arguments:\n", argc);
    for(int i = 0; i < argc; i++) {
        printf("arg%d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Sample output when invoking `./yourprogram Hello World!`:

```
You have entered 3 arguments:
arg0: ./yourprogram
arg1: Hello
arg2: World!
```

## Deep Dive

Once upon a time in the Unix era, terminal command lines were the go-to. Nowadays, GUIs are king, but command line arguments are far from extinct—think scripts, automated tasks, or complex applications parameters.

Arguments in C land through two parameters in `main()`: `argc` (argument count) and `argv` (argument vector). `argc` tells you how many arguments there are, while `argv` is an array of strings holding the actual arguments, `argv[0]` being the program's name.

There are alternatives like `getopt()` for Unix-like systems, which can parse options and their arguments nicely. Also, libraries like `argp` give you a helping hand for more complex parsing scenarios.

The nitty-gritty involves knowing about pointers and arrays, as `argv` is an array of character pointers—essentially a series of strings. When chomping through the command line, your program treats spaces as argument separators, unless quotes are your friends enclosing a single argument.

## See Also

- GNU C Library Manual's section on [Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [Wikipedia's entry on Command-line interface](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)
- ["C Command Line Arguments" on tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
