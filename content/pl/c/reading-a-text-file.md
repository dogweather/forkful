---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie pliku tekstowego to proces, w którym program komputerowy otwiera i odczytuje dane z pliku. Programiści robią to, żeby manipulować danymi, przetwarzać informacje, czy wczytywać ustawienia użytkownika.

## Jak to zrobić:
W C możemy użyć biblioteki `stdio.h`, aby otworzyć i przeczytać plik tekstowy. Przykładowy kod:

```C
#include <stdio.h>

int main() {
   char ch;
   FILE *fp;
   fp = fopen("example.txt", "r");

   if (fp == NULL) {
      perror("Błąd podczas otwierania pliku.\n");
      return(-1);
   }

   while((ch = fgetc(fp)) != EOF)
      printf("%c", ch);

   fclose(fp);

   return 0;
}
```
Podczas uruchomienia powyższy kod otworzy plik `example.txt` i wypisze jego zawartość na konsoli.

## W głąb tematu
Początkowo, wczesne wersje języka C nie miały łatwego dostępu do plików. Dopiero po wielu latach powstała biblioteka `stdio.h`.
Alternatywą dla `fopen` i `fclose` są funkcje `open` i `close` z biblioteki `unistd.h`, które dają większą kontrolę nad plikiem, ale są mniej proste w użyciu.
Funkcja `fopen` otwiera plik i zwraca wskaźnik na strukturę reprezentującą ten plik, natomiast `fgetc` zwraca kolejne znaki z pliku, aż do końca pliku (EOF).

## Zobacz także
Dokładniejsze wyjaśnienia oraz większa liczba przykładów do poczytania na:
- http://www.cplusplus.com/reference/cstdio/
- https://www.learn-c.org/en/File_Input/Output
- https://pl.wikibooks.org/wiki/C/Pliki