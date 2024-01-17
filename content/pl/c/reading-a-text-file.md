---
title:                "Odczytywanie pliku tekstowego"
html_title:           "C: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie pliku tekstowego to jedna z podstawowych czynności w programowaniu. Polega ona na wczytaniu zawartości pliku tekstowego do pamięci komputera w celu dalszej obróbki. Programiści często korzystają z tej funkcji, ponieważ pozwala ona na wczytanie dużej ilości danych z pliku do programu z łatwością.

## Jak to zrobić?

Aby odczytać plik tekstowy w języku C, potrzebujemy wskazać nazwę pliku oraz użyć odpowiednich funkcji. Przykładowy kod wygląda następująco:

```C
#include <stdio.h>

int main() {
    // nazwa pliku do odczytania
    char nazwa_pliku[] = "plik.txt";
    // utworzenie wskaźnika na zmienną przechowującą dane z pliku
    char dane[255];
    // użycie funkcji fopen do otworzenia pliku
    FILE *plik = fopen(nazwa_pliku, "r");
    if (plik == NULL) {
        // obsługa błędu w razie niepowodzenia
        printf("Nie udało się otworzyć pliku.");
        return 1;
    }
    // użycie funkcji fgets do odczytania danych z pliku i przypisanie ich do zmiennej
    while (fgets(dane, 255, plik) != NULL) {
        // wypisanie danych na ekran
        printf("%s", dane);
    }
    // zamknięcie pliku
    fclose(plik);
    return 0;
}
```

Przykładowe dane z pliku "plik.txt" mogą wyglądać tak:

```
To jest przykładowy plik tekstowy.
Pierwsza linijka.
Druga linijka.
Trzecia linijka.
```

Po uruchomieniu program, otrzymamy następujący wynik:

```
To jest przykładowy plik tekstowy.
Pierwsza linijka.
Druga linijka.
Trzecia linijka.
```

## Zanurzenie w temat

Odczytywanie pliku tekstowego jest jednym z podstawowych sposobów na wczytanie danych do programu. Inną alternatywą jest odczytywanie danych z innych formatów, takich jak pliki binarne lub baz danych. Jednak pliki tekstowe są najbardziej uniwersalne i powszechnie stosowane w programowaniu.

W języku C mamy do wyboru kilka funkcji do odczytywania plików tekstowych: `fread`, `fgets`, `fscanf` oraz `gets`. Każda z tych funkcji ma inne zastosowanie i wymaga innej składni, dlatego warto dokładnie zapoznać się z dokumentacją.

Warto również pamiętać o prawidłowym zabezpieczaniu programu przed błędami, takimi jak brak dostępu do pliku lub brak wystarczającej ilości pamięci do wczytania danych.

## Zobacz też

- [Dokumentacja języka C](https://en.cppreference.com/w/c)
- [Funkcje do obsługi plików w języku C](https://en.cppreference.com/w/c/io)
- [Przykładowe pliki tekstowe do odczytania](https://www.geeksforgeeks.org/file-handling-c-classes/)