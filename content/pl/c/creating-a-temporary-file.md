---
title:                "Tworzenie pliku tymczasowego"
html_title:           "C: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie pliku tymczasowego to proces, w którym programista tworzy plik, który istnieje tylko czasowo i jest używany do przechowywania danych lub wyników w trakcie wykonywania programu. Programiści często tworzą tymczasowe pliki, aby uniknąć modyfikacji lub przejścia przez wiele kroków, które mogą być potrzebne do wykonania danej czynności.

## Jak to zrobić:

### Przykład 1:
```C
#include <stdio.h>

int main() {
    FILE *temp_file = tmpfile(); //tworzenie tymczasowego pliku
    fprintf(temp_file, "To jest tymczasowy plik\n");
    fputs("zawierający dane\n", temp_file);
    fclose(temp_file); //zamykanie pliku
    return 0;
}
```
**Output:**
Tymczasowy plik będzie zawierał tekst "To jest tymczasowy plik" oraz "zawierający dane".

### Przykład 2:
```C
#include <stdio.h>

int main() {
    FILE *temp_file = fopen("temp.txt", "w+"); //tworzenie pliku tymczasowego o nazwie "temp.txt"
    fputs("To jest tymczasowy plik", temp_file);
    fseek(temp_file, 0, SEEK_SET); //ustawianie wskaźnika na początek pliku
    char buffer[50];
    fscanf(temp_file, "%s", buffer); //czytanie zawartości pliku i zapisanie jej w buforze
    printf("%s\n", buffer); //wypisywanie zawartości bufora
    fclose(temp_file); //zamykanie pliku
    return 0;
}
```
**Output:**
Tymczasowy plik "temp.txt" będzie zawierał tekst "To jest tymczasowy plik" i zostanie wypisany na ekran.

## Głębsze nurkowanie:

Tworzenie tymczasowego pliku jest praktykowane od dawna, gdyż zapewnia wiele korzyści. Głównym powodem jest uniknięcie zaburzenia istniejących plików lub zmiany danych, które mogą być potrzebne do późniejszego wykorzystania. Alternatywą dla tworzenia tymczasowego pliku może być użycie pamięci podręcznej, jednak może to być mniej wydajne. Implementacja tworzenia tempfile w języku C jest zależna od systemu operacyjnego, ale ogólnie korzysta z funkcji fopen() i fclose().

## Zobacz również:

[Funkcja tmpfile() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)

[Główna strona języka C](https://www.cprogramming.com/)