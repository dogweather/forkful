---
title:                "Praca z plikami CSV"
html_title:           "C: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinieneś rozważyć pracę z plikami CSV w języku C? Pliki CSV są powszechnie używane w celu przechowywania i udostępniania danych, więc umiejętność pracy z nimi jest niezbędna dla programisty.

## Jak to zrobić

Jeśli chcesz pracować z plikami CSV w języku C, potrzebujesz odpowiednich narzędzi i technik. W tym przykładowym kodzie będziesz miał okazję nauczyć się składni C i sposobów na manipulację danymi w plikach CSV.

```
/* Przykładowy kod w języku C do pracy z plikami CSV */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    // Utworzenie wskaźnika pliku
    FILE *fp;

    // Otwarcie pliku w trybie "read"
    fp = fopen("dane.csv", "r");

    // Zmienne do przechowywania danych z pliku
    char imie[100], nazwisko[100], stanowisko[100];

    // Pętla odczytująca dane z pliku i wyświetlająca je na ekranie
    while (fscanf(fp, "%[^,],%[^,],%[^\n]\n", imie, nazwisko, stanowisko) != EOF)
    {
        printf("Imię: %s\n", imie);
        printf("Nazwisko: %s\n", nazwisko);
        printf("Stanowisko: %s\n", stanowisko);

        printf("-------------------------\n");
    }

    // Zamknięcie pliku
    fclose(fp);

    return 0;
}
```

Przykładowy plik "dane.csv" może wyglądać następująco:
```
Imię,Nazwisko,Stanowisko
Jan,Kowalski,Programista
Anna,Nowak,Analityk
Marcin,Piotrowski,Designer
```

Przykładowy output:
```
Imię: Jan
Nazwisko: Kowalski
Stanowisko: Programista
-------------------------
Imię: Anna
Nazwisko: Nowak
Stanowisko: Analityk
-------------------------
Imię: Marcin
Nazwisko: Piotrowski
Stanowisko: Designer
-------------------------
```

## Deep Dive

Programowanie w języku C pozwala na wykorzystanie wielu funkcji i bibliotek do pracy z plikami CSV. Oprócz funkcji `fopen()` i `fclose()`, możesz także używać `fprintf()` i `fscanf()` do zapisywania i odczytywania danych w formacie CSV.

Jednym z ważnych elementów pracy z plikami CSV jest również umiejętność obsługi błędów. Gdy wykorzystujesz funkcję `scanf()` do odczytu danych z pliku, musisz zadbać o to, aby dane były zgodne z formatem określonym w funkcji. W przeciwnym razie może wystąpić błąd i program nie będzie w stanie poprawnie odczytać danych.

## Zobacz też

- Dokumentacja języka C: https://en.cppreference.com/w/c
- Wprowadzenie do pracy z plikami w języku C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- Biblioteka `stdio.h` w języku C: https://www.tutorialspoint.com/c_standard_library/stdio_h.htm