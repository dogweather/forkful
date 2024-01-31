---
title:                "Praca z plikami CSV"
date:                  2024-01-19
simple_title:         "Praca z plikami CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
CSV to pliki 'Comma-Separated Values', proste i popularne w przenoszeniu danych tabelarycznych. Programiści używają ich do importu, eksportu, analizy danych i ich szybkiego przetwarzania.

## How to: (Jak to zrobić:)
Przykład czytania CSV w C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAX_LINE_SIZE 1024

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Nie można otworzyć pliku.\n");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_SIZE];
    while (fgets(line, MAX_LINE_SIZE, fp)) {
        // Usuń znak nowej linii z wczytanej linii.
        line[strcspn(line, "\n")] = 0;

        char *token = strtok(line, ",");
        while(token) {
            printf("%s ", token);
            token = strtok(NULL, ",");
        }
        printf("\n");
    }
    fclose(fp);
    return EXIT_SUCCESS;
}
```
Wyjście (output) będzie reprezentacją danych z pliku `data.csv` wydrukowaną w konsoli.

## Deep Dive (Dogłębna analiza)
CSV powstało w latach `70 i jest formatem tekstowym, co oznacza prostotę i szerokie wsparcie. Istnieją alternatywnie: JSON, XML, ale CSV jest wydajniejsze dla dużych zbiorów prostych danych. W C brak biblioteki standardowej dla CSV - musisz samodzielnie zarządzać pamięcią i strukturami danych.

## See Also (Zobacz również)
- Do nauki i eksperymentów użyj tutoriali online jak [tutorialspoint.com](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm) na temat `strtok()`.
- Specyfikacja CSV oferowana przez [RFC 4180](https://tools.ietf.org/html/rfc4180).
- Szukaj na [Stack Overflow](https://stackoverflow.com/questions/tagged/c+csv) dla przykładów i pomocy w precyzowaniu kodu do specyficznych wymagań.
