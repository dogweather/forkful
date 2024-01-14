---
title:    "C recipe: Deleting characters matching a pattern"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a specific pattern is a common task in many C programming scenarios. Whether it's to manipulate user input or remove unnecessary elements from a string, understanding how to delete these characters can greatly improve the efficiency of your code.

## How To

To delete characters matching a pattern in C, we can use the `strchr()` function from the standard library. This function takes in two arguments - a string and a character to search for - and returns a pointer to the first occurrence of that character in the string.

Using `strchr()`, we can loop through the string and delete any characters that match our desired pattern. Let's take a look at an example:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "Hello, World!";
    char pattern = 'l';

    while (strchr(str, pattern) != NULL) // Continue looping as long as there is a match
    {
        char *ptr = strchr(str, pattern); // Get the pointer to the first occurrence of pattern
        strcpy(ptr, ptr + 1); // Delete the character by copying the rest of the string over it
    }

    printf("%s", str); // Output: Heo, Word!
    return 0;
}
```

In this example, we are deleting all the occurrences of the letter 'l' in the string "Hello, World!". We do this by continuously looping until `strchr()` no longer returns a pointer to 'l', and then using `strcpy()` to delete the character.

You can also modify this code to delete entire words or phrases by using `strstr()` instead of `strchr()`.

## Deep Dive

While understanding how to use functions like `strchr()` and `strcpy()` is crucial for deleting characters matching a pattern, it's also important to understand the underlying concept of pointers in C.

When we use `strchr()` to find the first occurrence of a character, it returns a pointer to that character in memory. By using `strcpy()` to copy the rest of the string over that character, we are essentially moving all the characters in the string one step to the left, effectively "deleting" the character.

It's also worth noting that `strchr()` and `strstr()` are case-sensitive, so make sure to take that into account when implementing these functions in your code.

## See Also

- [C documentation on strchr()](https://www.cplusplus.com/reference/cstring/strchr/)
- [C documentation on strcpy()](https://www.cplusplus.com/reference/cstring/strcpy/)
- [GeeksforGeeks article on Deleting character(s) from a string in C](https://www.geeksforgeeks.org/delete-characters-string-matching-given-string/)
- [Programiz tutorial on Strings in C](https://www.programiz.com/c-programming/c-strings)

By understanding the fundamentals of pointers and these useful string manipulation functions, you can easily delete characters matching a pattern in your C programs and make them more efficient. Happy coding!