---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:34:26.660318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Concatenation blends strings together. It's handy for constructing messages, paths, or any data that's text-driven.

## How to: (Як це зробити:)
### Using strcat():
```C
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Вітаємо, ";
    char source[] = "Україна!";
    
    strcat(destination, source);
    printf("%s\n", destination); // Вітаємо, Україна!
    
    return 0;
}
```

### Manual concatenation:
```C
#include <stdio.h>

int main() {
    char str1[] = "Слава ";
    char str2[] = "Україні!";
    char combined[50];
    
    int i = 0, j = 0;
    // Copy str1 to combined
    while (str1[i] != '\0') {
        combined[i] = str1[i];
        i++;
    }
    
    // Append str2 to combined
    while (str2[j] != '\0') {
        combined[i + j] = str2[j];
        j++;
    }
    
    combined[i + j] = '\0'; // Null-terminate the combined string
    printf("%s\n", combined); // Слава Україні!
    
    return 0;
}
```

## Deep Dive (Поглиблений аналіз):
The current C standard (ISO/IEC 9899:2018) doesn't change much about string manipulation. Programmers have been concatenating strings since C's inception. Options vary:

- `strcat()` and `strncat()` come from the standard library, `string.h`. They are simple but risky due to buffer overflow.
- Manual concatenation is buffer overflow safe if done correctly, but it's verbose. It entails copying characters one by one, then appending a null terminator.
- Modern alternatives? Consider using safer functions like `strncat()` or BSD's `strlcat()` if available.
- Some languages abstract away the complexities, but in C, handling strings manually teaches important principles of memory management.

## See Also (Дивіться також):
- C11 Standard Documentation (ISO/IEC 9899:2018): https://www.iso.org/standard/74528.html 
- GNU C Library Reference Manual: https://www.gnu.org/software/libc/manual/
- Learn C Programming - TutorialsPoint: https://www.tutorialspoint.com/cprogramming/index.htm 
- Secure Coding in C and C++: https://www.cert.org/secure-coding/
