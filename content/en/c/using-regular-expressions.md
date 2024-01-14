---
title:    "C recipe: Using regular expressions"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool in the world of programming, allowing developers to quickly and efficiently manipulate text and data. They can help with tasks such as input validation, data scraping, and text parsing. Understanding how to use regular expressions can greatly enhance your coding skills and increase your productivity.

## How To

To use regular expressions in C, you need to include the `<regex.h>` header file in your program. This gives you access to the `regex` functions that perform pattern matching and manipulation. Let's look at a simple example that searches for a specific pattern in a string:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    char pattern[] = "a[0-9]";
    char string[] = "Apple is my favorite fruit, but I also like a6fan and a2orange.";

    // Compile the regular expression
    if (regcomp(&regex, pattern, 0) != 0) {
        printf("Invalid regular expression.\n");
        return 1;
    }

    // Search for the pattern in the string
    int result = regexec(&regex, string, 0, NULL, 0);
    if (result == 0) {
        printf("Pattern found.\n");
    } else if (result == REG_NOMATCH) {
        printf("Pattern not found.\n");
    } else {
        printf("Error while searching for pattern.\n");
    }

    // Clean up
    regfree(&regex);

    return 0;
}
```

In this example, we are searching for a pattern that starts with the letter "a" followed by a single digit. The output of this program will be:

```
Pattern found.
```

This is just a simple example, but regular expressions can become much more complex and powerful. You can use metacharacters to specify a variety of patterns and perform different operations on the matched text.

## Deep Dive

Regular expressions allow you to search, extract, and manipulate strings in a very precise and efficient way. Let's take a deeper look at some of the features and functions that are available in the `<regex.h>` library.

### Matching Characters

One of the most basic uses of regular expressions is to match a specific set of characters in a string. As we saw in our example above, we used a pattern that starts with the letter "a" and is followed by a digit. This pattern will match strings such as "a1", "a2", "a3", and so on. Here are a few more examples to further illustrate this concept:

- `cat` - This pattern will match the word "cat" exactly as it appears in a string.
- `c[a-zA-Z]t` - This pattern will match any word that starts with "c" followed by a letter in the range of a-z or A-Z, and ends with "t". So it would match words like "cat", "cot", "cut", "coat", etc.
- `gr[ae]y` - This pattern will match both "gray" and "grey".

### Metacharacters

Metacharacters are special characters that have a specific meaning within regular expressions. They allow you to specify more complex patterns and perform different operations on the matched text. Here are a few commonly used metacharacters and their meanings:

- `.` - Matches any single character.
- `^` - Matches the beginning of a string.
- `$` - Matches the end of a string.
- `[]` - Matches any character within square brackets.
- `[^]` - Matches any character except those within square brackets.
- `*` - Matches the preceding element 0 or more times.
- `+` - Matches the preceding element 1 or more times.
- `?` - Matches the preceding element 0 or 1 times.
- `()` - Groups patterns together.
- `\` - Escapes the next character, allowing you to match special characters.

For a complete list of metacharacters and their meanings, you can refer to the [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html).

### Functions in `<regex.h>`

The `<regex.h>` header file provides a variety of functions for working with regular expressions. These functions all start with the prefix `reg` and have different parameters and return values. Here are some commonly used functions:

- `regcomp()` - This function compiles a regular expression pattern and creates a corresponding `regex_t` object that can be used for matching.
- `regexec()` - This function searches for a pattern within a string and returns a match if found.
- `regerror()` - This function converts error codes into a user-friendly string.
- `regfree()` - This function frees the memory allocated for a `