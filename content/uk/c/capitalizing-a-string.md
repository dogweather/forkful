---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та чому?)
Capitalizing a string means converting all its letters to uppercase. Programmers do this to normalize input, emphasize text, or meet coding standards.

## How to: (Як це зробити)
Here's a simple C function to capitalize a string:

```c
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "привіт світ!";
    capitalizeString(text);
    printf("Capitalized: %s\n", text);
    return 0;
}
```
Sample output:
```
Capitalized: ПРИВІТ СВІТ!
```

## Deep Dive (Поглиблене вивчення)
Capitalizing strings isn't new—people have been doing it in writing for emphasis for centuries. In C, it became common with the standardization of functions like `toupper` in the `<ctype.h>` library.

Alternatives to the `toupper` function include writing your custom function or using libraries like `glib` for more complex linguistic rules.

It's crucial to handle multi-byte encodings correctly, like UTF-8 in international applications. Simple `toupper` might not work as expected with such encodings, requiring additional libraries like `wchar.h` for wide characters.

## See Also (Дивіться також)
- C Standard Library reference: https://en.cppreference.com/w/c/header
- GNU C Library (glibc) documentation: https://www.gnu.org/software/libc/manual/
- UTF-8 handling in C: https://www.gnu.org/software/libunistring/
