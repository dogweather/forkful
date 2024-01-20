---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Regular Expressions in C Programming

## What & Why?
Regular expressions (regex) are sequences of characters that form search patterns. They are used in programming for matching, searching, and manipulating text strings. 

## How to: 
Using regex in C programming typically involves the `regex.h` library. Let's dive right in and write a simple program that checks if the user's input is a valid email:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int return_value;
    char email[100];

    return_value = regcomp(&regex, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$", 0);
    
    printf("Enter your email: ");
    fgets(email, 100, stdin);

    return_value = regexec(&regex, email, 0, NULL, 0);
    
    if(!return_value)
        printf("Valid Email.\n");
    else if(return_value == REG_NOMATCH)
        printf("Invalid Email.\n");
    else
        printf("An error occurred.\n");

    regfree(&regex);

    return 0;
}
```
This code compiles the regex for a valid email and uses `regexec()` to match it against the user's input.

## Deep Dive
Regular expressions have been around since the early 1960s, and have become an integral part of text parsing and manipulation in almost all high-level programming languages. 

In C programming, one could argue there are simpler methods for some specific tasks. However, regex provides powerful versatility unmatched by methods such as `strstr()` or `strpbrk()`. 

In terms of implementation details, note that `regcomp()` compiles a regular expression into a form that `regexec()` can use, and `regfree()` frees up any memory that `regcomp()` allocated.

## See Also
For more advanced usage of regular expressions in C programming, refer to the following links: 
- GNU C Library: [Regular Expression Library](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- Comparison of Regular Expression Engines: [Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_regular_expression_engines)