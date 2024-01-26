---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:34:08.352217-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
Concatenating strings in C involves combining two or more strings into one. We do this to construct messages, paths, or to process text dynamically.

## How to: (איך לעשות את זה)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Hello, ";
    char str2[] = "World!";
    
    strcat(str1, str2); // Concatenates str2 to str1
    printf("%s\n", str1); // Outputs the concatenated string
    
    return 0;
}
```
Sample Output:
```
Hello, World!
```

## Deep Dive (צלילה עמוקה)
In the early days, string concatenation in C was manual - tricky and error-prone. `strcat` saves us from that. Always allocate enough space for the combined strings plus the null terminator, to avoid buffer overflows. Alternatives to `strcat` include `strncat`, which limits the number of concatenated characters, and more robust functions like `snprintf` for complex string operations.

## See Also (ראה גם)
- The C Standard Library manual for `strcat` and `strncat`: https://en.cppreference.com/w/c/string/byte/strcat
- A guide to safer string concatenation in C: https://www.owasp.org/index.php/C-Based_Toolchain_Hardening_Cheat_Sheet
- Information on buffer overflows and how to prevent them: https://owasp.org/www-community/vulnerabilities/Buffer_Overflow
