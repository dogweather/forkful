---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) search, match, and manipulate strings. Programmers use them for text validation, searching, and transformations, speeding up text processing tasks.

## How to:
C doesn't have built-in regex support, but you can use libraries like `regex.h`. Here's a simple pattern match.

```c
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int result;
    char *pattern = "^hello";
    char *text = "hello world";

    // Compile regex
    result = regcomp(&regex, pattern, REG_EXTENDED);

    if (result) {
        printf("Regex compilation failed.\n");
        return 1;
    }

    // Execute regex
    result = regexec(&regex, text, 0, NULL, 0);
    
    // Check for match
    if (!result) {
        printf("Match found.\n");
    } else if (result == REG_NOMATCH) {
        printf("No match.\n");
    } else {
        printf("Regex execution failed.\n");
    }

    // Free up regex
    regfree(&regex);

    return 0;
}
```
Sample Output:
```
Match found.
```

## Deep Dive
Regular expressions have been in use since the 1950s, proliferating with Unix's `ed` and `grep`. Alternatives in C include string function libraries and custom parsers, but regex is more versatile. Under the hood, `regex.h` implements regex functionality, usually through NFA (Non-deterministic Finite Automaton) or DFA (Deterministic Finite Automaton) engines.

## See Also
- POSIX standard: https://pubs.opengroup.org/onlinepubs/9699919799/
- Regular Expressions (regex) tutorial: https://www.regular-expressions.info/
- POSIX regex in C: http://man7.org/linux/man-pages/man3/regcomp.3.html
