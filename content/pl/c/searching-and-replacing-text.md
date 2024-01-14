---
title:                "C: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu tekstowym zwykle występuje potrzeba zmiany fragmentu tekstu w jednej lub wielu liniach kodu. To może być spowodowane błędami w pisowni, aktualizacją danych lub po prostu potrzebą dostosowania tekstu do konkretnych wymagań. W takich przypadkach przydatne jest narzędzie do wyszukiwania i zamiany tekstu. W tym artykule omówimy, jak zrealizować to zadanie za pomocą języka programowania C.

## Jak to zrobić

Aby przeprowadzić wyszukiwanie i zamianę tekstu w języku C, potrzebujemy dwóch podstawowych elementów: funkcji `strstr()` i funkcji `strcpy()`. Funkcja `strstr()` służy do wyszukania ciągu znaków w danej napisie, a funkcja `strcpy()` do kopiowania tekstu z jednego miejsca do drugiego. W poniższym przykładzie użyjemy również funkcji `printf()`, aby wyświetlić rezultaty na ekranie.

```C
#include <stdio.h>
#include <string.h>

// Funkcja wyszukująca i zamieniająca tekst 
void searchAndReplace(char *text, char *search, char *replace) {
    char *textPtr;
    // Używamy pętli while, aby przeszukiwać cały napis
    while((textPtr = strstr(text, search))) {
        // Kopiujemy tekst przed szukanym ciągiem do nowej zmiennej
        char before[100];
        strncpy(before, text, textPtr-text);
        // Dodajemy znak końca linii
        before[textPtr-text] = '\0';

        // Teraz dodajemy tekst do zmiennej zastępczej
        char after[100];
        strcpy(after, replace);

        // Dodajemy tekst po szukanym ciągu do zmiennej zastępczej
        strcat(after, textPtr+strlen(search));

        // Kopiujemy tekst ze zmiennej po zmianie do zmiennej oryginalnej
        // W ten sposób dokonujemy zamiany tekstu
        strcpy(text, before);
        strcat(text, after);
    }
}

int main() {
    // Przykładowy napis do przeszukania i zmiany
    char text[] = "Przykladowy napis do przeszukania i zmiany.";

    // Wywołujemy funkcję searchAndReplace(), przekazując jako argumenty 
    // napis do przeszukania, szukany ciąg i ciąg zastępczy
    searchAndReplace(text, "zmiany", "modyfikacji");

    // Wyświetlamy zmieniony napis na ekranie
    printf("%s", text);
    return 0;
}
```

Powyższy przykład wyświetli na ekranie: "Przykladowy napis do przeszukania i modyfikacji." Jak widać, używając funkcji `strstr()` i `strcpy()`, możemy łatwo przeprowadzić zamianę tekstu w napisie.

## Wnikliwe spojrzenie

W języku C istnieje wiele innych funkcji, które mogą być przydatne w przeprowadzaniu zmian tekstowych. Na przykład funkcja `strncpy()` kopiująca n znaków z jednego ciągu do drugiego lub funkcja `strtok()` służąca do dzielenia tekstu na mniejsze części. Zawsze warto zapoznać się z możliwościami danego języka programowania i wybrać najlepszą funkcję do konkretnego zadania.

## Zobacz też

- [Oficjalna dokumentacja języka C](https://www.cplusplus.com/reference/cstring/strstr/)
- [Przewodnik po funkcjach string w C](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Wyszukiwanie i zamiana w tekście w języku C++](https://www.educative.io/edpresso/how-to-find-and-replace