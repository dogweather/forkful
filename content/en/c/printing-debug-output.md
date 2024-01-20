---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debug Output in C: A Practical Guide

## What & Why?

Printing debug output in C is the practice of using print functions to track the flow of a program and inspect variables. Programmers do this for easier troubleshooting and code validation.
  
## How To:

C uses several functions for debug printing, the most popular being `printf()`. Let's dive into an example:

```C
#include <stdio.h>

int main() {
    for(int i = 0; i < 10; i++) {
        printf("Iteration No.: %d\n", i);
    }
    return 0;
}
```

Here, we're simply printing out the number of iteration happening each time our loop runs. When you run this, you'll typically see:

```C
Iteration No.: 0
Iteration No.: 1
Iteration No.: 2
... up to 9
```

## Deep Dive

Historically, debug output traces back to assembly language programming and software diagnostics. It offers a quick and effective way of tracking bugs in code. 

There are other alternatives to printf, such as `fprintf()`, which prints to a file, and `sprintf()`, which prints to a string.

C does not inherently support debug levels like other languages do (e.g., DEBUG, ERROR, WARN); however, you could create your own logging function that supports levels.

As an implementation detail, keep in mind that `printf()` function has a return type. It returns the number of characters printed or a negative value if an error occurs. 

## See Also

Interested in other debugging methods in C? Here are a few additional resources:

- [How to Debug C Program using gdb in 6 Simple Steps](https://www.thegeekstuff.com/2010/03/debug-c-program-using-gdb/)
- [Mastering printf Debugger](https://beej.us/guide/bgc/html/#the-printf-debugger)