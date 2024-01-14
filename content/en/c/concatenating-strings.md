---
title:    "C recipe: Concatenating strings"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Strings are an essential component of any programming language. They allow us to store and manipulate text, making our programs more dynamic and interactive. One common operation when working with strings is concatenation, which involves combining two or more strings into one. In this blog post, we will explore why and how to concatenate strings in the C programming language.

## How To

To concatenate strings in C, we will use the `strcat()` function from the `string.h` library. This function takes two arguments - the first is the destination string, and the second is the source string. The `strcat()` function will append the source string to the end of the destination string, modifying the original destination string.

Let's see an example of how to use the `strcat()` function to concatenate two strings:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char destination[30] = "Hello";
    char source[] = "World!";
    strcat(destination, source);
    printf("%s", destination);
    return 0;
}
```

The output of this program will be `HelloWorld!`, as the `strcat()` function has appended the source string "World!" to the end of the destination string "Hello."

We can also use the `strncat()` function if we want to concatenate a specific number of characters from the source string. This function takes a third argument, which specifies the maximum number of characters to concatenate.

Let's see an example of using the `strncat()` function to concatenate only three characters from the source string:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char destination[10] = "Cat";
    char source[] = "Lion";
    strncat(destination, source, 3);
    printf("%s", destination);
    return 0;
}
```

The output of this program will be `CatLio`, as only the first three characters of the source string "Lion" were concatenated to the destination string "Cat."

## Deep Dive

Under the hood, when we use the `strcat()` or `strncat()` functions, we are essentially copying the characters from the source string and adding them to the end of the destination string. The functions will stop when it reaches the end of the source string or when the specified number of characters have been concatenated.

It is important to note that when using these functions, we must ensure that the destination string has enough space to hold the additional characters. Otherwise, we risk overwriting other data in memory and causing unexpected results.

## See Also

- [C Programming Language - Strings](https://www.programiz.com/c-programming/c-strings)
- [String Functions in C](https://overiq.com/c-programming-101/string-functions-in-c/)
- [String Concatenation in C](https://www.guru99.com/c-string-functions.html#5)