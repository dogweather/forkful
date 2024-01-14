---
title:                "C: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest częstym zadaniem w wielu aplikacjach C. Pliki tymczasowe są używane do przechowywania danych, które są potrzebne tylko przez krótki czas lub które nie są potrzebne po zamknięciu programu. Mogą też być wykorzystywane jako tymczasowe „magazyny” dla wyników pośrednich w procesach obliczeniowych.

## Jak to zrobić

Tworzenie plików tymczasowych w języku C jest dość proste. Kod poniżej pokazuje przykład funkcji, która tworzy plik tymczasowy i zapisuje do niego pewne dane. Prosimy zwrócić uwagę na komentarze w kodzie, które wyjaśniają poszczególne kroki:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Tworzy unikalną nazwę dla pliku tymczasowego, który będzie zawierał
    // wygenerowaną przez program liczbę
    char *temp_file = tmpnam(NULL);
    
    // Aby utworzyć plik tymczasowy używamy funkcji fopen()
    FILE *fp = fopen(temp_file, "w");
    
    // Sprawdzamy, czy udało się utworzyć plik
    if (fp == NULL)
    {
        printf("Błąd podczas tworzenia pliku tymczasowego\n");
        return 1;
    }
    
    // Zapisujemy dane do pliku
    fprintf(fp, "Tworzenie pliku tymczasowego w języku C");
    
    // Zamykamy plik
    fclose(fp);
    
    // Komunikat o pomyślnym utworzeniu pliku
    printf("Plik tymczasowy utworzony: %s\n", temp_file);
    
    return 0;
}

```
Przykładowy wynik:

```
Plik tymczasowy utworzony: /tmp/tmpH7Qc4x
```

## Głębsza analiza

W języku C możemy skorzystać z kilku różnych funkcji do tworzenia plików tymczasowych. Jedną z popularniejszych jest funkcja `tmpnam()` użyta w naszym przykładzie. Funkcja ta generuje unikalną nazwę dla pliku tymczasowego w formacie `/tmp/tmpXXXXXX`, gdzie `X` oznacza losową literę lub cyfrę.

Możemy też użyć funkcji `mkstemp()`, która tworzy plik tymczasowy i zwraca jego deskryptor. Dzięki temu możemy bezpośrednio pracować z plikiem, bez konieczności używania funkcji `fopen()`.

W przypadku chęci utworzenia pliku tymczasowego w danym katalogu, możemy użyć funkcji `mktemp()`, która umożliwia określenie ścieżki do utworzenia pliku.

Tworzenie plików tymczasowych w języku C jest bardzo przydatną umiejętnością, którą warto poznać podczas nauki programowania w tym języku.

## Zobacz także

* [Dokumentacja funkcji tmpnam()](https://www.tutorialspoint.com/c_standard_library/c_function_tmpnam.htm)
* [Przykładowy kod tworzący plik tymczasowy w języku C++](https://www.geeksforgeeks.org/tmpnam-function-in-c-c/)
* [Dokumentacja funkcji mkstemp()](https://www.tutorialspoint.com/c_standard_library/c_function_mkstemp.htm)