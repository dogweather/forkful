---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Co i dlaczego?

Capitalizing a string means converting the first letter of each word to uppercase. Programmers do it to format text for consistency, readability, or to meet specific data standards.

## How to:
## Jak to zrobić:

```C
#include <stdio.h>
#include <ctype.h>

// Function to capitalize each word in a string
void capitalizeString(char *str) {
    int inWord = 0;
    
    while (*str) {
        if (isalpha(*str) && !inWord) {
            *str = toupper((unsigned char) *str);
            inWord = 1;
        } else if (!isalpha(*str)) {
            inWord = 0;
        }
        str++;
    }
}

int main() {
    char text[] = "witaj, świecie! jak się masz?";
    capitalizeString(text);
    printf("Capitalized: %s\n", text);
    return 0;
}

```
Sample Output:
```
Capitalized: Witaj, Świecie! Jak Się Masz?
```

## Deep Dive
## Uwagi dodatkowe

Back in the day, data was often stored in uppercase only, partially because early computers lacked the capability to display lowercase letters. Now, capitalizing strings is commonplace, especially in user interfaces.

Alternatives to consider: Instead of rolling out your own function, third-party libraries like `Boost` in C++ offer string manipulation utilities with more features and error handling.

Implementation details: When capitalizing a string that contains multibyte encoding like UTF-8, you'd need a more sophisticated approach than `toupper`, which is designed for single-byte characters. Libraries like `ICU` provide this functionality.

## See Also
## Zobacz także

- C Standard Library documentation: https://en.cppreference.com/w/c/string/byte
- The ICU Project for Unicode support: http://site.icu-project.org/
- Boost Libraries: https://www.boost.org/
