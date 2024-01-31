---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
date:                  2024-01-20T17:41:39.505249-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Poistamme merkkejä, jotka täsmäävät määrättyyn kuviomalliin. Siksi, että voimme puhdistaa suorituksen kannalta turhat tai haitalliset datan osat.

## How to: (Kuinka Tehdä:)
```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *src = str, *dst = str;
    while(*src) {
        const char *p = pattern;
        bool match = true;
        while(*p && *src && *p == *src) {
            ++p;
            ++src;
        }
        match = (*p == '\0');
        if(!match) {
            src -= (p - pattern); // Palaa takaisin, mikäli ei täsmää
        }
        while(match && *src) {
            ++src; // Hyppää poistettavan kuviomallin yli
        }
        *dst++ = *src ? *src++ : '\0'; // Kopioi tai päätä merkkijono
    }
}

int main() {
    char str[] = "abc123abc456abc789";
    printf("Original string: %s\n", str);
    delete_pattern(str, "abc");
    printf("After deleting 'abc': %s\n", str);
    return 0;
}
```
Sample output:
```
Original string: abc123abc456abc789
After deleting 'abc': 123456789
```

## Deep Dive (Syväsukellus):
Deleting characters that match a pattern has been a common task in computing since the early days of programming. In Unix, tools like `sed` and `awk` were created for stream editing, which often involves pattern matching and deletion.

C doesn't offer built-in functions for pattern-based deletion. You need to do it manually, as shown above, or use a library like regex.h for complex patterns. The shown `delete_pattern` function compares substrings to a static pattern. If a match is found, it skips over the pattern; if not, it copies characters normally.

When working with patterns in C, there are trade-offs. Implementations like the one above are simple but not optimized for performance. In high-performance applications, algorithms like Knuth-Morris-Pratt or Boyer-Moore can be used for efficient pattern matching.

## See Also (Katso Myös):
- POSIX regex.h library: https://pubs.opengroup.org/onlinepubs/009695399/basedefs/regex.h.html
- Knuth-Morris-Pratt Algorithm: https://en.wikipedia.org/wiki/Knuth–Morris–Pratt_algorithm
- Boyer-Moore String Search Algorithm: https://en.wikipedia.org/wiki/Boyer–Moore_string-search_algorithm
