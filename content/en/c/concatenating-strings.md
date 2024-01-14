---
title:    "C recipe: Concatenating strings"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
In programming, it is common to come across situations where we need to combine or merge two or more strings together. This process is known as string concatenation and is a crucial aspect of string manipulations. Concatenating strings allows us to create larger, more complex strings that can be used for a variety of purposes. In this blog post, we will explore the concept of string concatenation in the C programming language.

## How To
To concatenate strings in C, we first need to understand the built-in string handling functions. The most commonly used function for concatenation is `strcat()`, which stands for string concatenation. This function takes two strings as arguments and merges the second string at the end of the first one. Let's see an example of how this works:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str1[20] = "Hello ";
    char str2[10] = "World";

    strcat(str1, str2);

    printf("Concatenated string: %s\n", str1);

    return 0;
}

// Output:
// Concatenated string: Hello World
```

In this example, we declared two strings `str1` and `str2` and initialized them with some values. Then using the `strcat()` function, we merged `str2` at the end of `str1` to create a new string "Hello World". It's important to note that the first argument of `strcat()` must be a large enough string to hold the concatenated result. Otherwise, it can cause a buffer overflow, resulting in undefined behavior.

Apart from `strcat()`, we can also use the `strcpy()` function to concatenate strings. This function copies the contents of one string to another, effectively merging them. Let's see how this works in the example below:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str1[20] = "Hello ";
    char str2[10] = "World";

    strcpy(str1, str2);

    printf("Concatenated string: %s\n", str1);

    return 0;
}

// Output:
// Concatenated string: World
```

In this example, we copied the contents of `str2` to `str1`, resulting in the concatenated string "World". Unlike `strcat()`, `strcpy()` overwrites the contents of the first string, so make sure to use it with caution.

## Deep Dive
Under the hood, string concatenation in C is carried out by the `strcat()` function in a sequential fashion. It starts from the last character of the first string and copies each character from the second string until it reaches the null-terminator `\0`. This process continues until the second string is fully copied, resulting in a merged string. It is essential to ensure that the first string has enough memory to hold the concatenated result. Otherwise, it can lead to unexpected behaviors, such as segmentation faults.

## See Also
- [String concatenation in C - GeeksforGeeks](https://www.geeksforgeeks.org/strcat-function-in-c/)
- [String handling functions in C - Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C string functions - Programiz](https://www.programiz.com/c-programming/library-function/string.h)

By now, you have a good understanding of how string concatenation works in the C programming language. Remember to use it with caution and make sure to check for adequate memory allocation to avoid any potential errors. Happy coding!