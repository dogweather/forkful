---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:37:55.997146-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego?
Zamiana napisu na małe litery polega na przekształceniu wszystkich dużych liter na odpowiadające im małe. Programiści wykonują tę operację, by ujednolicić dane, co ułatwia porównywanie i przetwarzanie tekstów.

## How to:
Jak to zrobić:
Użyjemy standardowej funkcji `tolower()` z biblioteki `<ctype.h>`, by przejść przez string i zamienić każdą dużą literę na małą.

```C
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while(*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "Witaj Świecie!";
    toLowerCase(myString);
    printf("Po zmianie: %s\n", myString);
    return 0;
}
```

Wynik:
```
Po zmianie: witaj świecie!
```

## Deep Dive:
Więcej informacji:
Funkcja `tolower()` jest standardowa od czasów języka ANSI C. Działa na pojedynczych znakach, więc żeby przekształcić cały string, musisz przejść przez niego pętlą. Alternatywą jest użycie funkcji `strlwr()`, która nie jest częścią standardu C, ale dostępna w niektórych kompilatorach. Operacje na stringach są fundamentem obróbki tekstu, a możliwość ich ujednolicania otwiera drzwi do wyszukiwania, sortowania i wielu innych operacji.

## See Also:
Zobacz też:
- Dokumentacja na temat `tolower()`: https://en.cppreference.com/w/c/string/byte/tolower
- Ogólna wiedza o stringach w C: https://www.tutorialspoint.com/cprogramming/c_strings.htm
