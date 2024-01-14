---
title:                "C recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming. It allows us to quickly make changes to our code or data without having to manually go through each line. This saves us time and ensures that the changes are consistent throughout the file.

## How To

To search and replace text in C programming, we can use the `strchr()` and `strncpy()` functions from the standard C library. Here's an example of how we can use these functions to replace all occurrences of a specific character with another character:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello world!";
    char *ptr = strchr(str, 'o'); // returns a pointer to the first occurrence of 'o'

    while (ptr) {
        strncpy(ptr, "x", 1); // replaces the character at the pointer's position with 'x'
        ptr = strchr(ptr + 1, 'o'); // moves the pointer to the next occurrence of 'o'
    }

    printf("%s", str); // output: Hellx wxrld!
    return 0;
}
```

In this example, we used `strchr()` to find the first occurrence of the character 'o' in the string and then used `strncpy()` to replace it with 'x'. We then continued to use `strchr()` to find and replace all other occurrences of 'o' until the end of the string.

It's important to note that `strncpy()` includes the null terminator character in the replacement, so we can only replace one character at a time. If we want to replace a sequence of characters, we can use the `memcpy()` function instead.

## Deep Dive

The `strchr()` function takes in two arguments: a string and a character to search for. It returns a pointer to the first occurrence of the character in the string, or a NULL pointer if the character is not found. We can then use this pointer in conjunction with other standard string functions like `strncpy()`, `strcpy()`, or `strcat()` to make changes to the string.

On the other hand, the `strncpy()` function takes in three arguments: a pointer to the destination string, a pointer to the source string, and the number of characters to copy. It copies the specified number of characters from the source string to the destination string, including the null terminator character. This is why we used a length of 1 in our example when replacing a single character.

Additionally, the `strncpy()` function does not automatically add a null terminator character if the source string is shorter than the specified length. This is why we always need to make sure the destination string is large enough to hold the copied characters, and we manually add the null terminator at the end if necessary.

## See Also

- [strchr() function in C](https://www.geeksforgeeks.org/strchr-in-c/)

- [strncpy() function in C](https://www.geeksforgeeks.org/strncpy-c-library-function/)

- [String manipulation functions in C](https://www.geeksforgeeks.org/string-manipulation-in-c-with-examples/)