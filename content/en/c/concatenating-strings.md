---
title:                "Concatenating strings"
html_title:           "C recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in C refers to combining multiple strings into one. This is a common task in programming, as it allows us to manipulate and display text in different ways. Whether it's creating a dynamic message or building a file path, string concatenation is a valuable tool for any C programmer.

## How to:

The basic syntax for concatenating strings in C is to use the `strcat()` function, which is defined in the `string.h` header file. Here's an example of how to use it:

```
#include <stdio.h>
#include <string.h>

int main() {
    char first_name[] = "John";
    char last_name[] = "Doe";
    char full_name[20];

    strcat(full_name, first_name);
    strcat(full_name, " ");
    strcat(full_name, last_name);

    printf("Hello, %s!", full_name);

    return 0;
}
```

Output: 
```
Hello, John Doe!
```

Note that the `strcat()` function appends the second string to the end of the first string, so it's important to make sure the first string has enough space to accommodate the additional characters.

If you want to concatenate more than two strings, you can use multiple `strcat()` functions or use the `sprintf()` function, which allows you to specify a format string to combine multiple strings. Here's an example:

```
#include <stdio.h>
#include <string.h>

int main() {
    char string[20];

    sprintf(string, "The answer is %d.", 42);

    printf("%s", string);

    return 0;
}
```

Output:
```
The answer is 42.
```

## Deep Dive

In the early days of C, there was no `strcat()` function, so programmers had to write their own functions to concatenate strings. One common method was to use a loop to copy characters from one string to another until reaching the end of the first string, and then copying the second string. However, this method was not very efficient and could potentially cause buffer overflows.

Another alternative to `strcat()` is the `strcpy()` function, which copies one string to another. However, this method only works if the target string is empty, so it's not ideal for concatenating multiple strings.

In terms of implementation, the `strcat()` function works by searching for the null character at the end of the first string and then copying the characters of the second string to that position. It then adds a null character at the end to terminate the string. As a result, it's important to make sure the first string has enough space to accommodate the additional characters.

## See Also

- [The `string.h` header file in C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C `strcat()` function documentation](https://www.programiz.com/c-programming/library-function/string.h/strcat)