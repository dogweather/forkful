---
title:                "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Po co

C jest jednym z najpopularniejszych języków programowania na świecie, a umiejętność czytania plików tekstowych jest niezbędną umiejętnością dla każdego programisty. Ten blog post opowie Ci, dlaczego warto nauczyć się czytać pliki tekstowe w języku C.

## Jak to zrobić

Kiedy już opanujesz podstawy C, możesz przejść do nauki odczytywania plików tekstowych. W tym celu użyjesz funkcji `fopen()`, `fscanf()` i `fclose()`. Oto przykładowy kod:

```C
#include <stdio.h>

int main() {
    FILE *plik;
    char imie[20], nazwisko[20];
    int wiek;

    plik = fopen("dane.txt", "r");
    fscanf(plik, "%s %s %d", imie, nazwisko, &wiek);
    fclose(plik);

    printf("Witaj, %s %s! Masz %d lat.", imie, nazwisko, wiek);

    return 0;
}
```

Funkcja `fopen()` otwiera plik o podanej nazwie, `fscanf()` wczytuje dane z pliku, a `fclose()` zamyka plik. Prosty, prawda? Warto jednak pamiętać o poprawnym obsłużeniu ewentualnych błędów, na przykład gdy plik nie istnieje.

## Dogłębny opis

Czytanie plików tekstowych może być nieco bardziej skomplikowane, jeśli chcemy na przykład wczytać kilka linii tekstu lub dania numeryczne. W takim przypadku warto zapoznać się z funkcją `fgets()`, która wczytuje pojedynczą linię tekstu do tablicy znaków, oraz funkcją `strtok()`, która dzieli linię na mniejsze części, tzw. tokeny. Możesz też wykorzystać pętlę `while` i sprawdzać warunek końca pliku za pomocą funkcji `feof()`.

## Zobacz też

- [Dokumentacja języka C](https://pl.wikibooks.org/wiki/C)
- [Tutorial odczytywania i zapisywania plików w języku C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Poradnik czytania plików tekstowych w języku C](https://www.programiz.com/c-programming/c-file-input-output)