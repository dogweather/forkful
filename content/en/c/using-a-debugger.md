---
title:                "Using a debugger"
date:                  2024-01-25T20:50:27.608650-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using a debugger"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## What & Why?
A debugger is a tool that lets you inspect your C code while it runs, step by step, to hunt down bugs. Programmers use debuggers to understand how their code behaves, fix issues, and optimize performance without playing a guessing game.

## How to:
Say you’re working with a simple C program that calculates the factorial of a number, but there’s a glitch. To use a debugger like `gdb` (GNU Debugger), first compile with the `-g` flag to include debug info:

```c
// compile with: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // A simple check for negative input
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("The factorial of %d is %ld\n", number, result);
    return 0;
}
```

Then run it in gdb:

```shell
$ gdb ./factorial
```

Set a breakpoint at the `factorial` function and run the program:

```gdb
(gdb) break factorial
(gdb) run
```

When it hits the breakpoint, step through each line using `next` or `n` and inspect variables with `print` or `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Sample output will provide real-time values and program execution flow.

## Deep Dive
Debuggers have been around since the 1960s, evolving from simple monitors to complex, GUI-based applications. Old-school print-based debugging was common before mature debuggers were developed. Alternatives to `gdb` include `lldb`, `dbx`, or IDE-integrated debuggers like those in Visual Studio or CLion. 

When dealing with debuggers, implementation varies—some can catch runtime errors, examine memory, or even reverse the execution of a program. `gdb` can attach to running processes, allowing for debugging of already-running software, a boon for fixing live system bugs.

## See Also
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging with GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Debugging Techniques in C: http://www.cprogramming.com/debugging/debugging.html
