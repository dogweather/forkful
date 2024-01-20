---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie pliku tymczasowego to operacja zapisania danych do pliku, który nie jest przeznaczony do długotrwałego przechowywania. Programiści robią to, aby tymczasowo przechować dane i zapewnić ich bezpieczeństwo podczas pracy programu.

## Jak to zrobić?

Przykład kodu, który tworzy plik tymczasowy, wygląda tak:

```C
#include <stdio.h>
 
int main() {
    char temp_filename[] = "/tmp/fileXXXXXX";
    int fd = mkstemp(temp_filename);
 
    if (fd == -1) {
        perror("Nie można utworzyć pliku tymczasowego");
        return 1;
    }
 
    printf("Plik tymczasowy: %s\n", temp_filename);
 
    // Zrob coś z plikiem...
 
    close(fd);
 
    return 0;
}
```

Przykładowe wyjście programu:

```C
Plik tymczasowy: /tmp/filea7b8c9
```

## Dogłębne omówienie

Historia: Pliki tymczasowe mają swoje korzenie w UNIX-ie, gdzie były one stosowane w wielu sytuacjach związanych z zarządzaniem danymi.

Alternatywy: Inna funkcja, która tworzy plik tymczasowy, to tmpnam, ale jest ona uważana za niebezpieczną, ponieważ może prowadzić do luki bezpieczeństwa, znanego jako „sygnalizacja wyścigu”.

Szczegóły implementacji: Funkcja 'mkstemp' tworzy unikalny plik tymczasowy z prawami dostępu rw-------. Zwraca deskryptor pliku i zmienia templatę wejściową na nazwę pliku.

## Zobacz także

- 'man 3 mkstemp' - pełny opis funkcji i jej zastosowań.
- Książka "Programowanie w języku ANSI C" - Brian W. Kernighan, Dennis M. Ritchie
- [C Programming/Files - Wikibooks]("https://en.wikibooks.org/wiki/C_Programming/Files")
- [Creating Temporary Files securely - GNU C Library]("https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html")