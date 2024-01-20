---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case in C programming means changing all the uppercase characters in a string to lowercase. Programmers do it to ensure case-insensitive comparisons and searches within the string.

## How to:

Here is a simple example showing how to convert a string to lower case:

```C
#include <stdio.h>
#include <ctype.h>

void stringToLower(char s[]) {
    for(int i = 0; s[i]; i++){
        s[i] = tolower(s[i]);
    }
}

int main() {
    char str[] = "HELLO, WORLD!";
    stringToLower(str);
    printf("%s\n", str); // Output: hello, world!
    return 0;
}
```
In this code, a `for` loop is used to iterate over the characters in the string, and the `tolower` function from the `ctype.h` library is applied to each character.

## Deep Dive

Historically, case manipulation functions like `tolower` have been part of the C programming language standard since its inception in 1972. It falls under the `ctype.h` header file, which contains a set of functions used to classify characters by their types — alphabetic, numeric, etc.

The `tolower` function has a counterpart, `toupper`, which converts a character to uppercase.

One alternative to using the `ctype.h` library would be to manipulate the ASCII values of characters directly. In ASCII, the difference between an uppercase letter and its lowercase equivalent is 32. However, this method is not usually recommended due to its reliance on ASCII specifics and it's less readable than using the standard library functions.

## See Also:

1. [C Library - ctype.h](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
2. [C String manipulation functions](https://www.geeksforgeeks.org/string-handling-in-c/) 
3. [Converting Strings and Characters to Lowercase in C](https://pagefault.blog/2021/01/15/converting-strings-and-characters-to-lowercase-in-c/)