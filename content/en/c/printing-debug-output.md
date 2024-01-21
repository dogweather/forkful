---
title:                "Printing debug output"
date:                  2024-01-20T17:52:10.913058-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is like inserting little checkpoints in your code to spit out information, helping to figure out what the heck is going on in there. Programmers do this to pinpoint bugs or to ensure their code is doing what it's supposed to, step-by-step.

## How to:

Here's the rub—printing debug output is easy. The heavy lifter in C is `printf`. Check out this simple example:

```c
#include <stdio.h>

int main() {
    int loopCounter = 0;
    for(loopCounter = 0; loopCounter < 5; loopCounter++) {
        printf("Loop iteration: %d\n", loopCounter);
        // More complex code here.
    }
    printf("Loop finished.\n");
    // Rest of your code.
    return 0;
}
```

Running this will splash on your screen:

```
Loop iteration: 0
Loop iteration: 1
Loop iteration: 2
Loop iteration: 3
Loop iteration: 4
Loop finished.
```

Simple, right? Just remember to remove or comment out these lines when you're done so your console isn't cluttered.

## Deep Dive

Back in the days of yore, there wasn't any fancy Integrated Development Environment (IDE) debugger to hold your hand. Raw output to the terminal was what you had. Today, it's still gold for quick and dirty diagnostics.

Alternatives? Well, for heavy-duty debugging, you might toggle to using proper IDE debuggers, or logging facilities that offer more control.

`printf` is your go-to, but there's more under the hood. For example, `fprintf(stderr, ...)` can redirect your messages to the standard error stream, making them easier to separate from standard output.

Also, if performance matters, you might avoid logging in tight loops or consider compiling with macros that let you strip out debug code in production.

## See Also

- [GNU Debugger (GDB)](https://www.gnu.org/software/gdb/) for when you're ready to move past `printf`.
- [C Logging Libraries](https://www.slant.co/topics/1183/~best-logging-add-ons-for-c-programming) for structured logging.
- [Learn C The Hard Way](https://learncodethehardway.org/c/) for a deeper dive into the broader world of C programming.