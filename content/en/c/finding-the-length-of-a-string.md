---
title:                "C recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Strings are a fundamental data type in programming, and often times we need to know the length of a string. Whether it's for file handling, input validation, or sorting algorithms, being able to find the length of a string is a useful skill to have in your programming arsenal.

## How To
To find the length of a string in C, we can use the standard library function `strlen()` which is defined in the `string.h` header file. It takes in a string as a parameter and returns the length of the string as an integer.

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello World";
    int length = strlen(str);
    printf("The length of the string is: %d", length);
    return 0;
}
```

Output:
```
The length of the string is: 11
```

## Deep Dive
Now, let's take a deeper look at how `strlen()` actually works. The function iterates through each character of the string until it reaches the null character `\0` which denotes the end of the string. It then returns the number of characters it has iterated through. This means that the null character is not included in the length count.

It's important to note that `strlen()` only works for strings, not other data types like integers or floats.

## See Also
- [C String Functions](https://www.programiz.com/c-programming/string-functions)
- [C Standard Library](https://www.cplusplus.com/reference/clibrary/)
- [The basics of strings in C](https://www.geeksforgeeks.org/strings-in-c-2/)