---
date: 2024-02-03 17:50:07.213729-07:00
description: "How to: In C, strings are arrays of characters ending with a null character\
  \ (`\\0`). Unlike in higher-level languages, C does not provide a built-in string\u2026"
lastmod: '2024-03-13T22:45:00.504383-06:00'
model: gpt-4-0125-preview
summary: In C, strings are arrays of characters ending with a null character (`\0`).
title: Concatenating strings
weight: 3
---

## How to:
In C, strings are arrays of characters ending with a null character (`\0`). Unlike in higher-level languages, C does not provide a built-in string concatenation function. Instead, you use the `strcat()` or `strncat()` functions from the `<string.h>` library.

Here’s a simple example using `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";

    strcat(destination, source);

    printf("%s\n", destination);  // Output: Hello, World!
    return 0;
}
```

The `strcat()` function takes two arguments: the destination string (which must have enough space to hold the concatenated result) and the source string. It then appends the source string to the destination string.

For more control over the number of characters concatenated, `strncat()` is safer to use:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";
    int num = 3; // Number of characters to append

    strncat(destination, source, num);

    printf("%s\n", destination);  // Output: Hello, Wor
    return 0;
}
```

This limits the concatenation to the first `num` characters of the source string, helping prevent buffer overflows.

## Deep Dive
The functions `strcat()` and `strncat()` have been part of the C standard library since its inception, reflecting the language's low-level nature that requires manual management of strings and memory. Unlike many modern programming languages that treat strings as first-class objects with built-in concatenation operators (such as `+` or `.concat()`), C's approach requires a more in-depth understanding of pointers, memory allocation, and potential pitfalls like buffer overflows.

While `strcat()` and `strncat()` are widely used, they are often criticized for their potential to create security vulnerabilities if not used carefully. Buffer overflows, where data exceeds the memory allocated, can lead to crashes or be exploited for arbitrary code execution. As a result, programmers are increasingly turning to safer alternatives, such as `snprintf()`, which provides more predictable behavior by limiting the number of characters written to the destination string based on its size:

```c
char destination[50] = "Hello, ";
char source[] = "World!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

This method is more verbose but significantly safer, highlighting a shift in C programming practices towards prioritizing security and robustness over brevity.

Despite these challenges, string concatenation in C is a foundational skill, crucial for effective programming in the language. Understanding its nuances and associated risks is key to mastering C programming.
