---
title:                "Extracting substrings"
html_title:           "C recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Substring extraction is a useful concept in programming that involves selecting a portion of a larger string. This can come in handy when working with large text data or parsing through user input. Programmers often use this technique to manipulate or extract specific information from strings.

## How to:

To extract a substring in C, we first need to understand the syntax for strings and characters. Strings in C are denoted by an array of characters, enclosed within double quotes. Characters, on the other hand, are denoted by a single letter or a numerical ASCII value, enclosed within single quotes. Here's an example:

```C
char str[] = "Hello, world!";
char ch = 'o';
```

To extract a substring from the string `str` that contains the character `ch`, we can use the `strstr()` function from the `string.h` library.

```C
char *sub = strstr(str, ch);
printf("Substring found: %s\n", sub);
```

This will return the substring `o, world!` and print it to the console. If the character `ch` is not found in the string `str`, the function will return `NULL`.

We can also use the `strtok()` function to extract substrings based on a delimiter. Delimiters are characters that separate different parts of a string. Using the same example string `str`, we can extract the first word by specifying the space character as the delimiter.

```C
char *word = strtok(str, " ");
printf("First word: %s\n", word);
```

This will print `Hello,` to the console. We can continue to use the `strtok()` function to extract the remaining words from the string.

## Deep Dive

Substring extraction has been a part of programming languages for a long time, with the first instance appearing in the famous string manipulation function `strchr()` in the C language. However, with the introduction of more modern languages, there are now alternative ways to extract substrings such as regular expressions or built-in functions like `slice` and `substring` in languages like JavaScript.

In the C language, there are several functions available for extracting substrings, including `strstr()`, `strtok()`, `strchr()`, and `strpbrk()`. These functions have different purposes and can be used depending on the desired outcome. For example, `strstr()` searches for a substring within a string, while `strtok()`tokenizes a string based on a delimiter.

## See Also

To learn more about string manipulation in C, you can check out the official documentation [here](https://www.gnu.org/software/libc/manual/html_node/String-Manipulation.html).

Other related sources include:

- [C String.h Reference](https://www.programiz.com/c-programming/library-function/string.h).
- [C Library - <string.h>](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C Programming – String Operations](https://www.tutorialspoint.com/cprogramming/c_string_handling.htm)