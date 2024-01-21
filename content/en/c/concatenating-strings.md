---
title:                "Concatenating strings"
date:                  2024-01-20T17:34:09.949811-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means sticking them end-to-end to form a new string. Programmers do it to combine text in dynamic ways, like constructing messages or generating file paths.

## How to:

In C, you use the `strcat` function from `string.h` to concatenate strings. But watch out, you need a destination array large enough to hold the combined result.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "world!";

    // Concatenate `source` onto `destination`
    strcat(destination, source);

    // Output the concatenated string
    printf("%s\n", destination); // "Hello, world!"

    return 0;
}
```

Make sure your destination array doesn't overflow. That's your job, as C won't do it for you.

## Deep Dive

Concatenation has been a basic text operation since early computing. In C, functions like `strcat` and `strncat` (which limits the number of concatenated characters) do the heavy lifting. C doesn't manage memory for you, so remember to allocate enough space before you concatenate.

Alternatives? Oh, sure. If you're worried about buffer overflows, you can use `snprintf` instead. It's safer since it lets you specify the max size of the output buffer:

```C
char buffer[50];
snprintf(buffer, 50, "%s%s", "Hello, ", "world!");
```

As for the nitty-gritty, `strcat` works by finding the end of the first string and copying the second string there character by character. Simple, yet manual memory management makes it prone to errors like buffer overruns.

## See Also

- C Standard Library documentation for `strcat`: https://en.cppreference.com/w/c/string/byte/strcat
- Secure coding in C: https://wiki.sei.cmu.edu/confluence/display/c/SEI+CERT+C+Coding+Standard
- Learn more about buffer overflows: https://owasp.org/www-community/attacks/Buffer_overflow