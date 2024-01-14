---
title:    "C recipe: Extracting substrings"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in programming, especially when dealing with strings. It involves taking a portion of a string and using it as a separate entity. Whether you are working on data manipulation, text parsing, or simply formatting output, knowing how to extract substrings will save you time and make your code more efficient.

## How To

To extract substrings in C, we will use the `strncpy()` function. This function takes three parameters - the destination string, the source string, and the number of characters to be copied. Let's take a look at a simple example:

```
#include <stdio.h>
#include <string.h>

int main() {
    char source[] = "Hello World";
    char destination[6];

    strncpy(destination, source, 5);
    destination[5] = '\0';
    printf("Extracted substring: %s", destination);
    
    return 0;
}
```

In this example, we declare a source string containing the phrase "Hello World" and a destination string with a size of 6 (to accommodate for the 5 characters we want to extract plus a null terminator). Then, using `strncpy()`, we copy the first 5 characters from the source string to the destination string. Finally, we add a null terminator at the end and print out the extracted substring.

The output of this code will be "Hello", as the `strncpy()` function does not automatically add a null terminator.

## Deep Dive

The `strncpy()` function is a safer alternative to the commonly used `strcpy()` function, as it allows you to specify the number of characters to be copied. This helps prevent buffer overflows, which can lead to security vulnerabilities. 

It is important to note that `strncpy()` does not guarantee that the destination string will be null-terminated, as seen in our example. To ensure this, we manually add a null terminator after copying the desired characters.

Another important feature of `strncpy()` is that it does not automatically add extra padding if the source string is shorter than the specified number of characters. In our example, if we try to copy 10 characters from the source string "Hello World", the result will be "Hello W", as the function stops copying after encountering the null terminator. This is different from `strcpy()`, which will automatically add padding until the specified number of characters is reached.

## See Also

To learn more about using substrings in C, check out these resources:

- [String Manipulation in C](https://www.tutorialspoint.com/cprogramming/c_string_manipulation.htm)
- [Extracting Substrings in C using Pointers](https://iq.opengenus.org/extract-substring-in-string-with-c/)
- [Official Documentation for `strncpy()`](https://www.gnu.org/software/libc/manual/html_node/Copying-Strings-and-Arrays.html)