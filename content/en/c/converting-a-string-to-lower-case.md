---
title:                "Converting a string to lower case"
date:                  2024-01-20T17:37:49.094187-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a string to lower case"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lowercase means changing all upper-case letters to their lower-case counterparts. It's done for consistency, searches, comparisons, and sorting, where case-sensitivity can muck things up.

## How to:

In C, you'd generally loop through the string, converting each character. Here's a quick example:

```c
#include <stdio.h>
#include <ctype.h>

void toLowercase(char *str) {
    if (!str) return; // Safety check
    while (*str) {
        *str = tolower((unsigned char)*str); // Convert each char to lowercase
        str++; // Move to next char
    }
}

int main() {
    char myStr[] = "Hello, World!";
    toLowercase(myStr);
    printf("%s\n", myStr); // Outputs: hello, world!
    return 0;
}
```

## Deep Dive

Long ago, when computer memories were small, folks cared about each byte. Converting strings wasn't trivial; it saved space to default to one case. Now, it's less about space, more about functionality.

Why use `tolower` and not roll our own? The C standard library's got it. It handles oddities across different character sets and locales. Rolling your own? You'd probably miss edge cases. Also, using the standard library means less code to maintain.

Fun fact: Old ASCII had 32 as the magic number separating cases—add or subtract 32 to jump between 'A' and 'a'. With Unicode, not so simple.

Alternatives? Libraries. For the modern C programmer, libraries like GLib transform strings in a blink, handling UTF-8 and such, but that's overkill for ASCII strings.

## See Also

- C Standard Library Reference: <http://www.cplusplus.com/reference/cctype/tolower/>
- ASCII Table and Description: <https://www.asciitable.com/>
- GLib Unicode manipulation: <https://developer.gnome.org/glib/stable/glib-Unicode-Manipulation.html>
