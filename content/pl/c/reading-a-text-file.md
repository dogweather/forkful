---
title:                "C: Odczytywanie pliku tekstowego"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas programowania w C potrzebujemy odczytać dane z pliku tekstowego. Jest to ważna umiejętność, ponieważ pozwala nam na łatwiejsze manipulowanie danymi i uniknięcie wprowadzania tych danych ręcznie do programu. W tym artykule dowiesz się, dlaczego odczytywanie pliku tekstowego jest ważne i jak to zrobić w prosty sposób.

## Jak to zrobić

Aby odczytać plik tekstowy w C, musimy użyć funkcji `fopen()` do otwarcia pliku, a następnie funkcji `fscanf()` do odczytania danych z pliku. Oto przykładowy kod, który otwiera plik "dane.txt" i wypisuje jego zawartość na ekranie:

```C
#include <stdio.h>

int main() {
    FILE* plik = fopen("dane.txt", "r");
    char slowo[100];

    while (fscanf(plik, "%s", slowo) != EOF) {
        printf("%s ", slowo);
    }

    fclose(plik);
    return 0;
}
```

W powyższym przykładzie użyliśmy pętli `while` i funkcji `fscanf()` do odczytywania danych z pliku do zmiennej `slowo`. Następnie wypisaliśmy tę zmienną na ekranie za pomocą funkcji `printf()`. Nie zapomnij o zamknięciu pliku za pomocą funkcji `fclose()`.

Spróbuj zmodyfikować powyższy kod, aby wczytywał inne typy danych (np. liczby całkowite) i wypisywał je na ekranie.

## Głębsza analiza

Podczas odczytywania pliku tekstowego w C, ważne jest, aby pamiętać o kilku rzeczach:

1. Zawsze sprawdzaj, czy plik został otwarty prawidłowo. Jeśli funkcja `fopen()` zwróci wartość NULL, oznacza to, że wystąpił błąd podczas otwierania pliku.

2. Zawsze pamiętaj o zamknięciu pliku za pomocą funkcji `fclose()` po zakończeniu pracy z plikiem. W przeciwnym razie może dojść do wycieku pamięci.

3. Uważaj na poprawność formatu danych. Jeśli używasz funkcji `fscanf()` do odczytywania danych, upewnij się, że format danych w pliku jest zgodny z tym, co oczekuje funkcja.

4. Należy pamiętać, że funkcja `fscanf()` pomija białe znaki, takie jak spacje i znaki nowej linii. Jeśli chcesz odczytać całą linię tekstu, użyj funkcji `fgets()`.

Mam nadzieję, że ten artykuł pomógł Ci zrozumieć dlaczego i jak odczytywać pliki tekstowe w C. Teraz możesz wykorzystać tę wiedzę do swoich kolejnych projektów!

## Zobacz także

- [Podstawy programowania w języku C](https://pl.wikibooks.org/wiki/C/Podstawy_programowania)
- [Dokumentacja języka C](https://devdocs.io/c/)