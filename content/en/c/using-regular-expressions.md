---
title:                "C recipe: Using regular expressions"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, or commonly known as "regex", are powerful tools used in programming to search and manipulate text based on a specified pattern. They are especially useful for tasks such as data cleaning, text parsing, and validation. Therefore, anyone working with text data can benefit from learning how to use regular expressions in their code.

## How To

To start using regular expressions in your C code, you will first need to include the regular expression library, `<regex.h>`. Then, you can declare a `regex_t` variable and compile your regular expression using the `regcomp()` function.

```C
#include <stdio.h>
#include <regex.h>

int main() {
    char str[] = "Hello, World!";
    regex_t regex;
    int ret;

    // Compile the regular expression 
    ret = regcomp(&regex, "Hell.", 0);
    if (ret) {
        fprintf(stderr, "Could not compile regex\n");
        return 1;
    }

    // Use the regexec() function to search for a match
    ret = regexec(&regex, str, 0, NULL, 0);
    if (!ret) {
        printf("Match found!");
    } else if (ret == REG_NOMATCH) {
        printf("No match found.");
    } else {
        fprintf(stderr, "Regex execution failed.");
    }

    // Free the compiled regular expression
    regfree(&regex);
    
    return 0;
}
```

The above code snippet will search for a match of the pattern "Hell." in the string "Hello, World!". If a match is found, it will print "Match found!", otherwise, it will print "No match found."

There are various functions available in the `<regex.h>` library for different operations such as replacing, extracting, and splitting text based on regular expressions. It is important to understand these functions and their usage before diving into more complex regular expressions.

## Deep Dive

Regular expressions have their own syntax and a few special characters to represent patterns. Some of the most commonly used special characters are:

- **`.`** - Matches any single character
- **`*`** - Matches zero or more occurrences of the previous character
- **`+`** - Matches one or more occurrences of the previous character
- **`?`** - Matches zero or one occurrence of the previous character
- **`^`** - Matches the beginning of a string
- **`$`** - Matches the end of a string
- **`[]`** - Matches any character within the brackets
- **`()`** - Groups characters together

Regular expressions also have modifiers that can be used to make the patterns more specific. For example, the **`*`** modifier is a "greedy" modifier and will match as many occurrences as possible, while the **`+`** modifier is a "possessive" modifier and will only match the first occurrence.

It is important to pay attention to these modifiers and understand their effects on the output of your regular expressions.

## See Also

- [Regular Expressions in C](https://www.regular-expressions.info/c.html)
- [C Programming Language](https://www.learn-c.org/)
- [Introduction to Regex on Codecademy](https://www.codecademy.com/learn/learn-regular-expressions)