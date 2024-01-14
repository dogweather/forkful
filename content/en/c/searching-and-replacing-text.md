---
title:                "C recipe: Searching and replacing text"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a crucial task in programming. It allows you to make changes to multiple instances of a particular string or character in a file, saving you time and effort. In this blog post, we will explore how to perform this task in C programming language.

## How To

To search and replace text in C programming, we will use the `strstr()` and `strncpy()` functions. The `strstr()` function searches for a substring within a string and returns a pointer to the first occurrence of the substring. The `strncpy()` function copies a specific number of characters from one string to another.

Let's take a look at an example to understand how to use these functions:

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[50] = "Hello, world!";
    char old_str[] = "world";
    char new_str[] = "everyone";

    char *ptr = strstr(str, old_str); //search for "world" in str

    strncpy(ptr, new_str, strlen(new_str)); //replace "world" with "everyone"

    printf("%s", str); //output: Hello, everyone!

    return 0;
}
```
In this code, we have a string `str` with the value "Hello, world!". We then use `strstr()` to find the substring "world" within `str` and store the pointer to it in `ptr`. Next, we use `strncpy()` to replace "world" with "everyone" at the location pointed by `ptr`. Finally, we print the modified string `str`, which now becomes "Hello, everyone!".

You can also use these functions in a loop to search and replace all occurrences of a string. Here's another example:

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[50] = "Hey there, how are you?";
    char old_str[] = "there";
    char new_str[] = "everyone";

    while (strstr(str, old_str) != NULL) { //loop until all occurrences are replaced
        char *ptr = strstr(str, old_str); //search for "there" in str
        strncpy(ptr, new_str, strlen(new_str)); //replace "there" with "everyone"
    }

    printf("%s", str); //output: Hey everyone, how are you?

    return 0;
}
```

## Deep Dive

The `strstr()` function has a prototype of `char *strstr(const char *haystack, const char *needle)`. It returns a pointer to the first occurrence of the `needle` string in the `haystack` string. If the `needle` string is not found, it returns `NULL`.

Similarly, the `strncpy()` function has a prototype of `char *strncpy(char *dest, const char *src, size_t n)`. It copies the first `n` characters of `src` to `dest`. If `src` is shorter than `n` characters, it appends null characters until a total of `n` characters have been written.

Both these functions are included in the standard library `<string.h>`.

## See Also

- [strchr() function in C](https://www.geeksforgeeks.org/strchr-c-implementation/)
- [strcpy() function in C](https://www.tutorialspoint.com/c_standard_library/c_function_strcpy.htm)
- [String manipulation in C](https://www.programiz.com/c-programming/c-strings)