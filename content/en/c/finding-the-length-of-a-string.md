---
title:                "Finding the length of a string"
html_title:           "C recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string is a common task in programming that involves determining the number of characters in a string. Programmers often need to find the length of a string to perform operations such as string manipulation, memory allocation, and comparison.

## How to:
To find the length of a string in C, we can use the built-in function `strlen()`. This function takes a string as an argument and returns an integer representing the length of the string. Here is an example of how we can use `strlen()` in our code:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char myString[] = "Hello World";
   int length = strlen(myString);
   printf("The length of my string is %d", length);
   return 0;
}
```

The output of this code will be: `The length of my string is 11`. As you can see, `strlen()` counts the number of characters in the string, including spaces and punctuation.

## Deep Dive:
In the early days of C, programmers had to calculate the length of strings manually using loops and conditional statements. This was a tedious and error-prone process, leading to the development of the `strlen()` function as a standard library function in the C programming language.

Other alternatives for finding the length of a string in C include using the function `sizeof()` which returns the size of a variable in bytes, or using pointers to count the number of characters in a string.

The `strlen()` function uses a simple algorithm known as the "linear time" algorithm, which calculates the length of a string by sequentially checking each character until a null terminator is reached. This makes it a very efficient and reliable method for finding the length of a string.

## See Also:
- [C string functions](https://www.programiz.com/c-programming/library-function/string.h)
- [Memory allocation in C](https://www.tutorialspoint.com/c_standard_library/c_function_malloc.htm)
- [Comparison operators in C](https://www.programiz.com/c-programming/c-operators)