---
title:    "C: Sprawdzanie istnienia katalogu."
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie czy istnieje katalog to bardzo przydatna umiejętność w programowaniu, która pozwala na sprawne zarządzanie plikami i folderami w systemie operacyjnym. W tym artykule dowiesz się, dlaczego warto nauczyć się tego zagadnienia.

## Jak to zrobić

```
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

int main() {
    // Wprowadź ścieżkę do katalogu
    char *path = "/sciezka/do/katalogu/";

    // Użyj funkcji stat() do sprawdzenia istnienia katalogu
    struct stat st;
    if (stat(path, &st) == 0) {
        // Jeśli katalog istnieje, wyświetl komunikat
        printf("Katalog %s istnieje.", path);
    } else {
        // Jeśli katalog nie istnieje, wyświetl komunikat
        printf("Katalog %s nie istnieje.", path);
    }

    return 0;
}
```

## Wnikliwiej

W celu sprawdzenia czy katalog istnieje, używamy funkcji `stat()`. Jest to funkcja, która pobiera informacje o pliku lub katalogu podanym w jej parametrze i zapisuje je do struktury `stat`. Jeśli funkcja zwróci wartość równą 0, oznacza to, że plik lub katalog istnieje. W przeciwnym przypadku, jeśli będzie to wartość różna od 0, oznacza to, że plik lub katalog nie istnieje.

Możemy również użyć funkcji `access()`, która sprawdza czy dany plik lub katalog jest dostępny w systemie. Możemy wykorzystać ją w połączeniu z flagą `F_OK`, aby sprawdzić czy plik lub katalog istnieje.

## Zobacz także

- https://www.tutorialspoint.com/c_standard_library/c_function_stat.htm
- https://www.tutorialspoint.com/c_standard_library/c_function_access.htm