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

## Why

Regular expressions, also known as regex, are a powerful tool for pattern matching in text. They can help programmers search, validate, and manipulate data efficiently, making it a valuable skill to learn in the world of coding.

## How To

To use regular expressions in C, you must include the `<regex.h>` header file in your code. Then, you can use the `regex` functions to perform operations like matching, searching, replacing, and splitting strings. Here's an example of code that checks if a string matches a specific pattern:

```C
#include <stdio.h>
#include <regex.h>

int main(){
    char *pattern = "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]";
    char *string = "123-4567";
    regex_t regex;

    if (regcomp(&regex, pattern, 0) != 0) {
        printf("Error compiling regex pattern\n");
        return 1;
    }

    if (regexec(&regex, string, 0, NULL, 0) == 0) {
        printf("Valid phone number format\n");
    } else {
        printf("Invalid phone number format\n");
    }

    regfree(&regex);
    return 0;
}
```

Output:

```
Valid phone number format
```

In this example, we use the `regcomp()` function to compile the regex pattern, and the `regexec()` function to match the pattern against the given string. If the string matches the pattern, the output will be "Valid phone number format". Otherwise, it will print "Invalid phone number format".

## Deep Dive

Regular expressions can be as simple or as complex as needed, depending on the task at hand. They consist of characters and special metacharacters that act as anchors, modifiers, or quantifiers. Here are some commonly used metacharacters and their corresponding functions:

- `.` - matches any single character.
- `^` - matches the beginning of a line.
- `$` - matches the end of a line.
- `*` - matches the preceding character 0 or more times.
- `+` - matches the preceding character 1 or more times.
- `?` - matches the preceding character 0 or 1 time.
- `()` - grouping characters.

There are also special sequences that represent common patterns, such as `\d` for a digit, `\w` for a word character, and `\s` for a whitespace character. You can also use quantifiers to specify how many times a character or group should be repeated, for example, `{3}` for exactly 3 times, or `{2,4}` for 2 to 4 times.

Regular expressions can be tested and experimented with using online tools like Regex101 or Regexr, where you can see the explanation and breakdown of your pattern as you type it.

## See Also

- [C Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- [GNU C Library - Regular Expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Regex101](https://regex101.com/)