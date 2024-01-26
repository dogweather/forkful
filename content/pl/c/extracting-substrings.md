---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:45:06.864031-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wyciąganie podciągów to sprowadzenie części łańcucha znaków do nowego ciągu. Programiści robią to, aby manipulować danymi, weryfikować formaty lub po prostu pokazać interesującą zawartość.

## How to: (Jak to zrobić:)
Extrahowanie podciągu można zrobić na wiele sposobów. Tutaj są przykłady:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Witaj, świecie!";
    char sub[8];
    
    // Przykład używający strncpy
    strncpy(sub, str + 7, 7);
    sub[7] = '\0'; // Dodanie znaku końca ciągu
    printf("Podciąg: %s\n", sub);

    return 0;
}

// Wyjście: Podciąg: świecie
```

Możesz też użyć wskaźników:

```c
#include <stdio.h>

int main() {
    const char *str = "Witaj, świecie!";
    const char *sub_start = str + 7;
    int len = 7;

    for(int i = 0; i < len; i++) {
        putchar(*(sub_start + i));
    }
    putchar('\n');

    return 0;
}

// Wyjście: świecie
```

## Deep Dive (Głębsza analiza)
Historia C jest stara jak świat komputerów — język powstał w latach 70-tych. W tamtych czasach, efektywność była kluczowa i operowanie na podciągach było niezbędne do optymalizacji. Alternatywami dla funkcji `strncpy` mogą być funkcje takie jak `snprintf` lub manualne kopiowanie za pomocą wskaźników. Ważne jest, aby zawsze pamiętać o zabezpieczeniu końca ciągu znakiem NULL, co zapobiega błędom odczytu.

Wersja standardowa C, którą obecnie używamy to C18. Choć podstawy pozostają te same, nowe standardy wprowadzają różne ulepszenia i funkcje. Ważne jest, aby być na bieżąco z dokumentacją i najlepszymi praktykami.

## See Also (Zobacz także)
- Strona dokumentacji `strncpy`: https://www.cplusplus.com/reference/cstring/strncpy/
- Tutorial dotyczący wskaźników w C: https://www.tutorialspoint.com/cprogramming/c_pointers.htm
- Podręcznik C18: https://www.iso-9899.info/wiki/The_Standard
