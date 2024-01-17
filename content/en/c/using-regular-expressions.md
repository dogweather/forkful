---
title:                "Using regular expressions"
html_title:           "C recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions are a powerful tool used by programmers to search, find, and manipulate text patterns in strings. They provide a concise and flexible way to perform complex tasks that would otherwise require many lines of code. Regular expressions are especially useful for tasks such as data validation, text parsing, and text substitution.

## How to:

To use regular expressions in your C code, you need to include the `<regex.h>` header file. Then, you can use the `regex` data type and related functions to create, match, and manipulate regular expressions. For example, the following code snippet shows how to use regular expressions to search for a specific pattern in a string:

```C
regex_t reg;
char str[] = "The quick brown fox jumps over the lazy dog";

int status = regcomp(&reg, "fox", 0);
if (status == 0) {
    int result = regexec(&reg, str, 0, NULL, 0);
    if (result == 0) {
        printf("'fox' was found in '%s'\n", str);
    } else if (result == REG_NOMATCH) {
        printf("No match found\n");
    } else {
        printf("Error executing regular expression\n");
    }
    regfree(&reg);
}
```

The output of the above code would be:

```
'fox' was found in 'The quick brown fox jumps over the lazy dog'
```

## Deep Dive

Regular expressions have been around since the 1950s and have evolved over the years. They are based on a mathematical concept called finite automata and have been widely adopted in many programming languages. Some popular alternatives to regular expressions in C include string manipulation functions from the `<string.h>` header, custom parsers, and third-party libraries.

In C, regular expressions are implemented using the POSIX standard, which defines the grammar and semantics of regular expressions. This standard allows for some variations and extensions, so it's important to refer to the documentation when using regular expressions in your code.

## See Also

To learn more about using regular expressions in C, check out the following resources:

- [The POSIX standard for regular expressions](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/regex.h.html)
- [A tutorial on regular expressions in C](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_71/rtref/regexec.htm)
- [The GNU C Library manual on regular expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)