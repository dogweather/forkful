---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:41:29.966334-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern in C is about finding and removing specific sequences of chars from strings. Programmers do it to sanitize input, manipulate text, or prep data for processing.

## How to:
To delete characters matching a pattern from a string, we can use the `strpbrk` function to find occurrences and `strcpy` or `memmove` to shuffle text around. Here's a quick example:

```c
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *match;
    while ((match = strpbrk(str, pattern)) != NULL) {
        memmove(match, match + 1, strlen(match));
    }
}

int main() {
    char text[] = "Hello, World! Today is 2023.";
    delete_pattern(text, "o3!");
    printf("%s\n", text); // Output: Hell, Wrld Tday is 22.
    return 0;
}
```
This code hunts down 'o', '3', and '!' chars, wiping them from the string.

## Deep Dive
Back in the day, before functions like `strpbrk` were standard, coders often wrote loops checking each char against a pattern—tedious but necessary. Today's C standard library removes a lot of that grunt work, but it's always good to understand what happens under the hood. 

`strpbrk` scans a string for the first match in a set of chars, and `memmove` safely moves bytes around, even if they overlap. This is different from `strcpy`, which can't handle overlapping memory without a hiccup.

Alternatives include regex libraries for complex patterns or manual looping for fine control. But it's always a trade-off between including external libraries or hand-crafting solutions for performance or memory constraints.

## See Also
- [C String Library Functions](https://www.cplusplus.com/reference/cstring/)
- [Regular Expressions in C](https://www.regular-expressions.info/posix.html)
