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

Capitalizing a string in C refers to the process of converting all letters in a string to uppercase. This can be useful in cases where case sensitivity is important, such as when comparing strings. Programmers often capitalize strings to ensure consistency and accuracy in their code.

## How to:

To capitalize a string in C, we can use the ```toupper()``` function from the standard library. Here's an example code snippet:

```C
#include <stdio.h>
#include <ctype.h>

char* capitalize(char* str) {
    char* ptr = str;
    while(*ptr != '\0') {
        *ptr = toupper(*ptr);
        ptr++;
    }
    return str;
}

int main() {
    char str[] = "hello world!";
    printf("Before capitalization: %s\n", str);
    printf("After capitalization: %s\n", capitalize(str));
    return 0;
}
```

The output of this code would be:

```
Before capitalization: hello world!
After capitalization: HELLO WORLD!
```

## Deep Dive:

Capitalization has been used since the early days of computer programming, when programmers had to use hardware switches to enter instructions. In those days, lowercase letters were not included in the character set, so capitalization was the only way to differentiate between letters. Today, it is more commonly used to ensure consistency since most modern programming languages are case-sensitive.

An alternative to using the ```toupper()``` function is to manually convert each letter to uppercase using ASCII codes. However, this method can be tedious and error-prone. Some C compilers also have built-in functions specifically for capitalization, such as ```strlwr()``` and ```strupr()```.

Implementing capitalization in C involves iterating through each character in the string and checking if it is a lowercase letter. If it is, the ```toupper()``` function is applied to convert it to uppercase. This process is repeated until all letters have been converted.

## See Also:

- [toupper() function in C](https://www.geeksforgeeks.org/toupper-in-cpp/)
- [strlwr() and strupr() functions in C](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [ASCII codes](https://www.asciitable.com/)