---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means turning all lowercase letters to uppercase. Programmers often capitalize strings for consistency, display formatting, or as part of data normalization processes.

## How to:
The C language doesn’t have a built-in function to capitalize strings. You'll typically loop through each character, capitalizing as you go:

```c
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "hello world!";
    capitalizeString(myString);
    printf("%s\n", myString);  // Output: HELLO WORLD!
    return 0;
}
```

## Deep Dive
In the early days of computing, operations on strings were basic and manual. C, developed in the early 1970s, reflects this with simple string manipulation functions in its standard library. The function `toupper` is designed to convert a single character to uppercase. It’s part of `<ctype.h>`, a header containing functions to test and map characters.

There are alternatives to looping through a string to capitalize it. Libraries like `libCStringUtils` offer more complex string operations, including capitalization. Some developers also write their own functions with features like local sensitivity.

Internally, ASCII characters have numeric equivalents, which differ by 32 between uppercase and lowercase. The `toupper` function uses this difference to convert characters. However, relying on ASCII values directly isn't advisable due to readability and localization issues.

## See Also
- C Standard Library documentation: https://en.cppreference.com/w/c/header
- ASCII Table and Description: http://www.asciitable.com/
- GNU Libc manual: https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html