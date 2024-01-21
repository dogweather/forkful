---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:53:40.062744-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Czytanie pliku tekstowego to proces pozyskiwania danych zapisanych na dysku. Programiści robią to, aby manipulować, analizować i wykorzystywać zawarte informacje.

## How to:
Otwieranie pliku, czytanie linia po linii, i zamknięcie pliku – oto prosty przykład:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *plik = fopen("przyklad.txt", "r");
    if (plik == NULL) {
        perror("Blad przy otwieraniu pliku");
        return EXIT_FAILURE;
    }

    char bufor[256];
    while (fgets(bufor, sizeof(bufor), plik)) {
        printf("%s", bufor);
    }

    fclose(plik);
    return EXIT_SUCCESS;
}
```

Przykładowe wyjście dla pliku `przyklad.txt` z treścią "Witaj w C!":
```
Witaj w C!
```

## Deep Dive
Czytanie plików tekstowych w C było możliwe od początków języka w latach 70. Alternatywne podejścia obejmują użycie funkcji `fread` dla większej kontroli nad procesem czytania. Ważne szczegóły: otwarcie pliku w odpowiednim trybie, obsługa błędów oraz zarządzanie zasobami poprzez zamknięcie pliku.

## See Also
- Manual dla funkcji fopen, fgets, fclose: https://man7.org/linux/man-pages/man3/fopen.3.html
- Dokumentacja C Standard Library: https://www.gnu.org/software/libc/manual/
- Wprowadzenie do obsługi plików: https://pl.wikibooks.org/wiki/C/Pliki