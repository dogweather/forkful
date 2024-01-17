---
title:                "Searching and replacing text"
html_title:           "C recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is the act of finding and changing specific words, phrases, or characters within a larger body of text. Programmers often use this technique to quickly make changes to their code, saving time and reducing manual error.

## How to:

To search and replace text in C, we can use the `strchr()` function. This function searches for a specific character within a string and returns a pointer to its location. We can then use this pointer to replace the character with a new one.

```
//Example code
char str[] = "Hello World";
char *ptr;
ptr = strchr(str, 'o');
*ptr = 'e';
printf("%s", str);

//Output
Helle World
```

We can also use the `strstr()` function to search for a specific string within another string. This function works similarly to `strchr()`, but searches for the entire string instead of just one character.

```
//Example code
char str[] = "Hello World";
char *ptr;
ptr = strstr(str, "World");
strcpy(ptr, "Universe");
printf("%s", str);

//Output
Hello Universe
```

## Deep Dive:

Before the advent of computers, searching and replacing text was a manual and time-consuming process. With the rise of programming languages, this task became much easier and more efficient.

Some alternatives to the `strchr()` and `strstr()` functions include `strtok()` and `regex`. `strtok()` breaks a string into smaller tokens, while `regex` allows for more complex matching patterns.

The implementation of `strchr()` and `strstr()` may differ slightly in different versions of C, but their fundamental purpose remains the same. It is important to remember to always check for error conditions and handle them appropriately when using these functions.

## See Also:

- [C Strchr() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [C Strstr() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [C Strtok() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)
- [C Regex tutorial](https://www.tutorialspoint.com/cprogramming/c_regular_expressions.htm)