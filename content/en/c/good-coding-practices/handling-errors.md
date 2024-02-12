---
title:                "Handling errors"
aliases: - /en/c/handling-errors.md
date:                  2024-02-03T17:50:05.622207-07:00
model:                 gpt-4-0125-preview
simple_title:         "Handling errors"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?

Handling errors in C involves detecting and responding to anomalous conditions that arise during program execution. Programmers do this to prevent bugs, crashes, and unpredictable behavior, ensuring the software functions reliably and efficiently under various scenarios.

## How to:

C does not have built-in support for exceptions like some other languages. Instead, it relies on a few conventional error-handing strategies, such as returning special values from functions and setting global variables like `errno`.

**Returning Special Values**

Functions can indicate errors by returning a specific value that is unlikely to be a valid result. Here's an example with integers:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Error case
    } else {
        *result = 1.0 / number;
        return 0; // Success
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Error: Division by zero.\n");
    } else {
        printf("The inverse is: %f\n", result);
    }
    
    return 0;
}
```

**Output:**
```
Error: Division by zero.
```

**Checking `errno`**

For library functions, especially those interacting with the system or OS (like file I/O), `errno` is set when an error occurs. To use it, include `errno.h` and check `errno` after a suspected failure:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Error opening file: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Output:**
```
Error opening file: No such file or directory
```

## Deep Dive

Historically, the C programming language's minimalistic design has excluded a built-in exception handling mechanism, reflective of its low-level, systems programming origins where maximum performance and close-to-the-metal control are critical. Instead, C adopts a more manual error handling approach that fits its philosophy of giving programmers as much control as possible, even at the cost of convenience.

While this approach aligns well with C's design goals, it can also lead to verbose error-checking code and the potential for missed error checks, which modern languages address with structured exception handling mechanisms. For instance, exceptions in languages like Java or C# allow for centralized error processing, making code cleaner and error management more straightforward. However, exceptions introduce their overhead and complexity, which might not be ideal for system-level programming where C shines.

Despite its rudeness, this manual error handling in C has informed the design of error management in many other languages, offering a model where the explicitness of error conditions can lead to more predictable and debuggable code. For critical systems, where failures must be managed gracefully, C's error handling paradigm—combined with modern best practices like error handling libraries and conventions—ensures robustness and reliability.
