---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Zapisywanie plików tekstowych to podstawowe działanie, pozwalające na trwałe zapisanie danych. Programiści używają tej metody, aby zachować wyniki, konfiguracje czy komunikaty.

## Jak to zrobić:

```c
#include <stdio.h>

int main() {
    FILE *plik = fopen("przykladowy.txt", "w");
    if (plik == NULL) {
        printf("Błąd przy otwieraniu pliku!\n");
        return 1;
    }
    fprintf(plik, "Cześć! To przykładowy tekst.\n");
    fclose(plik);

    return 0;
}
```

Po uruchomieniu programu, tworzy się plik "przykladowy.txt" z tekstem "Cześć! To przykładowy tekst."

## Dogłębniej

Zapisywanie plików tekstowych jest możliwe w C od początku języka (1972). Alternatywą może być użycie funkcji `fwrite`, ale `fprintf` jest lepsze do tekstów. Ważne jest zamknięcie pliku (`fclose`) by zapisać bufor i uniknąć wycieków pamięci.

## Zobacz też

- Tutorial C File I/O: https://www.cprogramming.com/tutorial/cfileio.html
- Dokumentacja standardu C11: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
- Przewodnik po funkcjach we/wy w C: https://en.cppreference.com/w/c/io
