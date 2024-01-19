---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenating Strings in C - A Handy Guide

## What & Why?

Simply put, concatenating in programming means joining two or more strings together into a single string. Developers often use this to effectively generate or manipulate textual data.

## How To:

Here's a basic example of concatenating, or joining, two strings using the strcat() function in C:

```C
#include <string.h>
#include <stdio.h>

int main() {
    char str1[100] = "Hello, ";
    char str2[] = "World!";

    strcat(str1, str2);

    printf("%s\n", str1);

    return 0;
}
```

Running this would output: 

```C
Hello, World!
```

This C code takes two separate strings - "Hello, " and "World!", and joins them together into a single string "Hello, World!".

## Deep Dive 

The ability to concatenate strings in C comes from string handling functions defined in the string.h library. Specifically, the strcat() function as shown above. 

And why strcat()? Its history is as old as the C language itself - Brian Kernighan and Dennis Ritchie introduced it in C's earliest versions. 

As for alternatives, we have the strncat() function, snprintf(), and sprintf(). They provide more flexibility and safety, especially when working with buffers and limiting string lengths.

However, it's noteworthy that C strings are null-terminated. This means whenever you use concatenation, the function needs to traverse until it finds the null character. So, if you're dealing with long strings, it can affect efficiency.

## See Also 

For more details and understanding, you can refer to these sources:

- strcat(): https://en.cppreference.com/w/c/string/byte/strcat
- strncat(): https://en.cppreference.com/w/c/string/byte/strncat
- C String handling functions: https://www.tutorialspoint.com/c_standard_library/string_h.htm