---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:16.864354-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Removing quotes from a string means stripping out any quotation marks—be it single ('') or double ("")—that are part of the string's content. Programmers do this to sanitize input, prepare data for further processing, or avoid syntax errors when dealing with file paths and commands in languages that use quotes to demarcate strings.

## How to:

Here's a C function that'll scrub those pesky quotes out of your strings:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanitized: %s\n", str);
    return 0;
}
```

Sample output:

```
Original: He said, "Hello, 'world'!"
Sanitized: He said, Hello, world!
```

## Deep Dive

Removing quotes from a string has been a task since the dawn of programming, where data hygiene was and still is key to avoiding errors (like SQL injection attacks) or making sure a string can safely be passed to systems that might confuse a quote for a control character. 

Historically, different languages handle this task differently—some have built-in functions (like `strip` in Python), while others, like C, require manual implementation due to its focus on giving developers lower-level control.

Alternatives include using library functions like `strpbrk` to find quotes or employing regular expressions (with libraries such as PCRE) for more complex patterns, although this might be overkill for simply removing quotes.

The implementation above simply scans through each character in the string, copying only non-quote characters to the write pointer location. This is efficient because it's done in-place without needing extra memory for the result string.

## See Also

- [C Standard Library Functions](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Understanding Pointers in C](https://www.learn-c.org/en/Pointers)
