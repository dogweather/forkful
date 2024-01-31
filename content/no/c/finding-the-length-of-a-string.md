---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:47:05.257203-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Å finne lengden på en streng betyr å telle antall tegn den inneholder. Programmerere trenger å vite dette for å manipulere tekstdata effektivt.

## How to:
I C bruker vi `strlen()` fra `string.h` biblioteket for å få lengden på en streng. Her er et eksempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char greeting[] = "Hei Verden!";
    int length = strlen(greeting);

    printf("Lengden av '%s' er %d.\n", greeting, length);

    return 0;
}
```

Kjører du dette, får du:
```
Lengden av 'Hei Verden!' er 11.
```

## Deep Dive
I de gamle C-dagene (før `string.h`), måtte man selv loope gjennom en streng for å finne lengden. Slik ser en manuell funksjon for å telle tegn ut:

```C
int stringLength(const char* str) {
    const char* ptr = str;
    while (*ptr) ++ptr;
    return ptr - str;
}
```

`strlen()` er enklere men vit at den teller til den treffer null-tegnet (`'\0'`). Det betyr at det ikke fungerer for strenger som ikke er null-terminerte. Alternativer inkluderer å bruke `std::string` i C++, som holder styr på lengden internt, eller å ha en tilpasset datastruktur som lagrer lengden.

## See Also
- C Standard Library Reference: https://en.cppreference.com/w/c/string/byte/strlen
- Discussion on `strlen()` vs `sizeof`: https://stackoverflow.com/questions/37538/how-do-i-determine-the-size-of-my-array-in-c
- About `std::string` in C++ (if you're curious about the neighbor): https://en.cppreference.com/w/cpp/string/basic_string
