---
title:                "Виділення підрядків"
date:                  2024-01-20T17:45:14.103242-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Витягування підрядків у C – це процес отримання частини рядка. Це важливо для аналізу даних, валідації вводу та обробки тексту.

## How to: (Як це зробити:)
```c
#include <stdio.h>
#include <string.h>

void extract_substring(char* string, int start, int length) {
    char substr[length + 1]; // +1 for the terminating null byte
    strncpy(substr, string + start, length);
    substr[length] = '\0'; // Ensure the substring is null-terminated
    printf("Extracted substring: '%s'\n", substr);
}

int main() {
    char my_string[] = "Привіт, світе!";
    extract_substring(my_string, 0, 6); // Виведе "Привіт"
    extract_substring(my_string, 8, 5); // Виведе "світе"
    return 0;
}
```
Sample Output:
```
Extracted substring: 'Привіт'
Extracted substring: 'світе'
```

## Deep Dive (Поглиблений Розбір)
The concept of strings didn't exist in early C; they were emulated using character arrays and pointers. The standard library added functions like `strncpy()` later. Alternatives to `strncpy()` include `sprintf()` for complex manipulations or manually copying characters with loops. Always watch out for buffer overflows and ensure every string is null-terminated. In modern C, bounds-check functions like `strncpy_s()` are preferred for safety.

## See Also (Дивіться також)
- C Standard Library reference for `strncpy`: https://en.cppreference.com/w/c/string/byte/strncpy
- Secure coding practices in C: https://www.securecoding.cert.org
- UTF-8 and Unicode in C programming: https://www.utf8everywhere.org/
